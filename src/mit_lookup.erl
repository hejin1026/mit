%目的:用于设备由于级联信息查找的设备.
%由于mit数据设计上的缺陷,导致查找一些级联设备非常麻烦,并且多个地方需要用到,提出来作为方法弥补
%比如:pon下onu的查询,目前pon和onu 的mit信息没有关联

-module(mit_lookup).

-created("hejin 2112-8-20").

-compile([export_all]).

-import(extbif, [to_binary/1]).

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

%pon下的onu
run(onus, {pon_dn, Dn}) ->
    case mnesia:dirty_read(entry, to_binary(Dn)) of
        [#entry{type = port, data = Data} = _Entry] ->
            Entries = mnesia:dirty_index_read(entry, mit_util:bdn(Dn), #entry.parent),
            {value,SlotNo} = dataset:get_value(slot_no, Data),
            {value,PortNo} = dataset:get_value(port_no, Data),
            filter_device_info(Entries, [{type, onu}, {slot_no, SlotNo}, {port_no, PortNo}]);
        [] ->
            []
    end.

run(onu, {olt_dn, Dn}, Items) ->
    Entries = mnesia:dirty_index_read(entry, Dn, #entry.parent),
   filter_device_info(Entries, Items);


filter_device_info([], _Items) ->
    [];
filter_device_info(Entries, Items) ->
    Entries2 = lists:filter(fun(Device) ->
                check_device_info(Items, Device, true)
            end, Entries),
%    ?INFO("get device :~p, ~n filter : ~p, ~n item: ~p", [Entries, Entries2, Items]),
    Entries2.



check_device_info(_Items, _Device, false) ->
    false;
check_device_info([], _Device, Result) ->
    Result;
check_device_info([{type, Value}|Items], Device, _Result) ->
    check_device_info(Items, Device, Device#entry.type == Value);
check_device_info([{slot_no, Value}|Items], Device, _Result) ->
    {value, No} = dataset:get_value(slot_no, Device#entry.data),
    check_device_info(Items, Device, No == Value);
check_device_info([{port_no, Value}|Items], Device, _Result) ->
    {value, No} = dataset:get_value(port_no, Device#entry.data),
    check_device_info(Items, Device, No == Value);
check_device_info([{onu_no, Value}|Items], Device, _Result) ->
    {value, No} = dataset:get_value(onu_no, Device#entry.data),
    check_device_info(Items, Device, No == Value).

