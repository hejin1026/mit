-define(OLT, 1).

-define(ONU, 2).

-define(EOC, 3).

-define(CPE, 4).

-define(DSLAM, 5).

-define(MIT_ADD, 1).

-define(MIT_DELETE, 2).

-define(MIT_UPDATE, 3).

%%example: {entry, <<"onu=1,olt=2">>, <<"onu:1">>, onu, [{id, 1}, {ip, "10.10.10.10"}]}
-record(entry, {dn, uid, ip, parent = <<"">>, type, data}).

-record(areas, {dn, uid, entranceid,branchid,townid, cityid, type, data}).


