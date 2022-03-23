% Alessio Disaro' 866247

initial(start).

final(q0).
final(q1).
final(ah0).
final(ah22).
final(ap1).
final(path0).
final(query1).
final(frag1).
final(tel0).
final(telu0).
final(mailto0).
final(mailu0).
final(mailh21).
final(mailh1).
final(news0).
final(zosah22).
final(zos0).
final(zosah0).
final(zospath1).
final(zosap1).
final(zospath4).
final(zosquery1).
final(zosfrag1).


% URI2
% Tel
% Scheme
delta(start, X, tels0, scheme) :-
    is_int_char(X, "t"), !.

delta(tels0, X, tels1, scheme) :-
    is_int_char(X, "e").

delta(tels1, X, tels2, scheme) :-
    is_int_char(X, "l").

delta(start, X, tels0, scheme) :-
    is_int_char(X, "T"), !.

delta(tels0, X, tels1, scheme) :-
    is_int_char(X, "E").

delta(tels1, X, tels2, scheme) :-
    is_int_char(X, "L").

% URI1?
delta(tels0, X, s0, scheme) :-
    is_not_int_char(X, "e"),
    is_not_int_char(X, "E"),
    identificatore(X).

delta(tels1, X, s0, scheme) :-
    is_not_int_char(X, "l"),
    is_not_int_char(X, "L"),
    identificatore(X).

delta(tels2, X, s0, scheme) :-
    is_not_int_char(X, ":"),
    identificatore(X).

% :
delta(tels2, X, tel0, other) :-
    is_int_char(X, ":").

% Userinfo
delta(tel0, X, telu0, user) :-
    identificatore(X).

delta(telu0, X, telu0, user) :-
    identificatore(X).

% Fax
delta(start, X, faxs0, scheme) :-
    is_int_char(X, "f"), !.

delta(faxs0, X, faxs1, scheme) :-
    is_int_char(X, "a").

delta(faxs1, X, faxs2, scheme) :-
    is_int_char(X, "x").

delta(start, X, faxs0, scheme) :-
    is_int_char(X, "F"), !.

delta(faxs0, X, faxs1, scheme) :-
    is_int_char(X, "A").

delta(faxs1, X, faxs2, scheme) :-
    is_int_char(X, "X").

% URI1?
delta(faxs0, X, s0, scheme) :-
    is_not_int_char(X, "a"),
    is_not_int_char(X, "A"),
    identificatore(X).

delta(faxs1, X, s0, scheme) :-
    is_not_int_char(X, "x"),
    is_not_int_char(X, "X"),
    identificatore(X).

delta(faxs2, X, s0, scheme) :-
    is_not_int_char(X, ":"),
    identificatore(X).

% :
delta(faxs2, X, tel0, other) :-
    is_int_char(X, ":").

% Mailto
delta(start, X, mails0, scheme) :-
    is_int_char(X, "m"), !.

delta(mails0, X, mails1, scheme) :-
    is_int_char(X, "a").

delta(mails1, X, mails2, scheme) :-
    is_int_char(X, "i").

delta(mails2, X, mails3, scheme) :-
    is_int_char(X, "l").

delta(mails3, X, mails4, scheme) :-
    is_int_char(X, "t").

delta(mails4, X, mails5, scheme) :-
    is_int_char(X, "o").

delta(start, X, mails0, scheme) :-
    is_int_char(X, "M"), !.

delta(mails0, X, mails1, scheme) :-
    is_int_char(X, "A").

delta(mails1, X, mails2, scheme) :-
    is_int_char(X, "I").

delta(mails2, X, mails3, scheme) :-
    is_int_char(X, "L").

delta(mails3, X, mails4, scheme) :-
    is_int_char(X, "T").

delta(mails4, X, mails5, scheme) :-
    is_int_char(X, "O").

% URI1?
delta(mails0, X, s0, scheme) :-
    is_not_int_char(X, "a"),
    is_not_int_char(X, "A"),
    identificatore(X).

delta(mails1, X, s0, scheme) :-
    is_not_int_char(X, "i"),
    is_not_int_char(X, "I"),
    identificatore(X).

delta(mails2, X, s0, scheme) :-
    is_not_int_char(X, "l"),
    is_not_int_char(X, "L"),
    identificatore(X).

delta(mails3, X, s0, scheme) :-
    is_not_int_char(X, "t"),
    is_not_int_char(X, "T"),
    identificatore(X).

delta(mails4, X, s0, scheme) :-
    is_not_int_char(X, "o"),
    is_not_int_char(X, "O"),
    identificatore(X).

delta(mails5, X, s0, scheme) :-
    is_not_int_char(X, ":"),
    identificatore(X).

% :
delta(mails5, X, mailto0, other) :-
    is_int_char(X, ":").

% Userinfo
delta(mailto0, X, mailu0, user) :-
    identificatore(X).

delta(mailu0, X, mailu0, user) :-
    identificatore(X).

% Host
delta(mailu0, X, mailh0, other) :-
    is_int_char(X, "@").

% IP
% N.
delta(mailh0, X, mailh2, host) :-
    is_int_digit_between(X, 0, 1).

delta(mailh2, X, mailh4, host) :-
    is_int_digit(X).

delta(mailh4, X, mailh5, host) :-
    is_int_digit(X).

delta(mailh0, X, mailh3, host) :-
    is_int_char(X, "2").

delta(mailh3, X, mailh4, host) :-
    is_int_digit_between(X, 0, 4).

delta(mailh3, X, mailh6, host) :-
    is_int_char(X, "5").

delta(mailh6, X, mailh5, host) :-
    is_int_digit_between(X, 0, 5).

delta(mailh0, X, mailh4, host) :-
    is_digit(X).

delta(mailh0, X, mailh5, host) :-
    is_digit(X).

delta(mailh5, X, mailh7, host) :-
    is_int_char(X, ".").

% N.
delta(mailh7, X, mailh8, host) :-
    is_int_digit_between(X, 0, 1).

delta(mailh8, X, mailh9, host) :-
    is_int_digit(X).

delta(mailh9, X, mailh10, host) :-
    is_int_digit(X).

delta(mailh7, X, mailh11, host) :-
    is_int_char(X, "2").

delta(mailh11, X, mailh9, host) :-
    is_int_digit_between(X, 0, 4).

delta(mailh11, X, mailh12, host) :-
    is_int_char(X, "5").

delta(mailh12, X, mailh10, host) :-
    is_int_digit_between(X, 0, 5).

delta(mailh7, X, mailh9, host) :-
    is_int_digit(X).

delta(mailh7, X, mailh10, host) :-
    is_int_digit(X).

delta(mailh10, X, mailh13, host) :-
    is_int_char(X, ".").

% N.
delta(mailh13, X, mailh14, host) :-
    is_int_digit_between(X, 0, 1).

delta(mailh14, X, mailh15, host) :-
    is_int_digit(X).

delta(mailh15, X, mailh16, host) :-
    is_int_digit(X).

delta(mailh13, X, mailh17, host) :-
    is_int_char(X, "2").

delta(mailh17, X, mailh15, host) :-
    is_int_digit_between(X, 0, 4).

delta(mailh17, X, mailh18, host) :-
    is_int_char(X, "5").

delta(mailh18, X, mailh16, host) :-
    is_int_digit_between(X, 0, 5).

delta(mailh13, X, mailh15, host) :-
    is_int_digit(X).

delta(mailh13, X, mailh16, host) :-
    is_int_digit(X).

delta(mailh16, X, mailh19, host) :-
    is_int_char(X, ".").

% N
delta(mailh19, X, mailh20, host) :-
    is_int_digit_between(X, 0, 1).

delta(mailh20, X, mailh21, host) :-
    is_int_digit(X).

delta(mailh21, X, mailh22, host) :-
    is_int_digit(X).

delta(mailh19, X, mailh23, host) :-
    is_int_char(X, "2").

delta(mailh23, X, mailh21, host) :-
    is_int_digit_between(X, 0, 4).

delta(mailh23, X, mailh24, host) :-
    is_int_char(X, "5").

delta(mailh24, X, mailh22, host) :-
    is_int_digit_between(X, 0, 5).

delta(mailh19, X, mailh21, host) :-
    is_int_digit(X).

delta(mailh19, X, mailh22, host) :-
    is_int_digit(X).

% Id
delta(mailh0, X, mailh1, host) :-
    id_host(X).

delta(mailh1, X, mailh1, host) :-
    id_host(X).

delta(mailh1, X, mailh25, host) :-
    is_int_char(X, ".").

delta(mailh25, X, mailh1, host) :-
    id_host(X).

% News
delta(start, X, newss0, scheme) :-
    is_int_char(X, "n"), !.

delta(newss0, X, newss1, scheme) :-
    is_int_char(X, "e").

delta(newss1, X, newss2, scheme) :-
    is_int_char(X, "w").

delta(newss2, X, newss3, scheme) :-
    is_int_char(X, "s").

delta(start, X, newss0, scheme) :-
    is_int_char(X, "N"), !.

delta(newss0, X, newss1, scheme) :-
    is_int_char(X, "E").

delta(newss1, X, newss2, scheme) :-
    is_int_char(X, "W").

delta(newss2, X, newss3, scheme) :-
    is_int_char(X, "S").

% URI1?
delta(newss0, X, s0, scheme) :-
    is_not_int_char(X, "e"),
    is_not_int_char(X, "E"),
    identificatore(X).

delta(newss1, X, s0, scheme) :-
    is_not_int_char(X, "w"),
    is_not_int_char(X, "W"),
    identificatore(X).

delta(newss2, X, s0, scheme) :-
    is_not_int_char(X, "s"),
    is_not_int_char(X, "S"),
    identificatore(X).

delta(newss3, X, s0, scheme) :-
    is_not_int_char(X, ":"),
    identificatore(X).

% :
delta(newss3, X, news0, other) :-
    is_int_char(X, ":").

% Host
% IP
delta(news0, X, mailh2, host) :-
    is_int_digit_between(X, 0, 1).

delta(news0, X, mailh3, host) :-
    is_int_char(X, "2").

delta(news0, X, mailh4, host) :-
    is_int_digit(X).

delta(news0, X, mailh5, host) :-
    is_int_digit(X).

% Id
delta(news0, X, mailh1, host) :-
    id_host(X).

% Zos
delta(start, X, zoss0, scheme) :-
    is_int_char(X, "z"), !.

delta(zoss0, X, zoss1, scheme) :-
    is_int_char(X, "o").

delta(zoss1, X, zoss2, scheme) :-
    is_int_char(X, "s").

delta(start, X, zoss0, scheme) :-
    is_int_char(X, "Z"), !.

delta(zoss0, X, zoss1, scheme) :-
    is_int_char(X, "O").

delta(zoss1, X, zoss2, scheme) :-
    is_int_char(X, "S").

% URI1?
delta(zoss0, X, s0, scheme) :-
    is_not_int_char(X, "o"),
    is_not_int_char(X, "O"),
    identificatore(X).

delta(zoss1, X, s0, scheme) :-
    is_not_int_char(X, "s"),
    is_not_int_char(X, "S"),
    identificatore(X).

delta(zoss2, X, s0, scheme) :-
    is_not_int_char(X, ":"),
    identificatore(X).

% :
delta(zoss2, X, zos0, other) :-
    is_int_char(X, ":").

% Authorithy
delta(zos0, X, zosa0, other) :-
    is_int_char(X, "/").

delta(zosa0, X, zosa1, other) :-
    is_int_char(X, "/").

% Userinfo
delta(zosa1, X, zosau0, user) :-
    identificatore(X).

delta(zosau0, X, zosau0, user) :-
    identificatore(X).

delta(zosau0, X, zosau1, other) :-
    is_int_char(X, "@").

% Host
% IP
% N.
delta(zosau1, X, zosah2, host) :-
    is_int_digit_between(X, 0, 1).

delta(zosah2, X, zosah4, host) :-
    is_int_digit(X).

delta(zosah4, X, zosah5, host) :-
    is_int_digit(X).

delta(zosau1, X, zosah3, host) :-
    is_int_char(X, "2").

delta(zosah3, X, zosah4, host) :-
    is_int_digit_between(X, 0, 4).

delta(zosah3, X, zosah6, host) :-
    is_int_char(X, "5").

delta(zosah6, X, zosah5, host) :-
    is_int_digit_between(X, 0, 5).

delta(zosa1, X, zosah2, host) :-
    is_int_digit_between(X, 0, 1).

delta(zosa1, X, zosah3, host) :-
    is_int_char(X, "2").

delta(zosa1, X, zosah4, host) :-
    is_int_digit(X).

delta(zosa1, X, zosah5, host) :-
    is_int_digit(X).

delta(zosau1, X, zosah4, host) :-
    is_int_digit(X).

delta(zosau1, X, zosah5, host) :-
    is_int_digit(X).

delta(zosah5, X, zosah7, host) :-
    is_int_char(X, ".").

% N.
delta(zosah7, X, zosah8, host) :-
    is_int_digit_between(X, 0, 1).

delta(zosah8, X, zosah9, host) :-
    is_int_digit(X).

delta(zosah9, X, zosah10, host) :-
    is_int_digit(X).

delta(zosah7, X, zosah11, host) :-
    is_int_char(X, "2").

delta(zosah11, X, zosah9, host) :-
    is_int_digit_between(X, 0, 4).

delta(zosah11, X, zosah12, host) :-
    is_int_char(X, "5").

delta(zosah12, X, zosah10, host) :-
    is_int_digit_between(X, 0, 5).

delta(zosah7, X, zosah9, host) :-
    is_int_digit(X).

delta(zosah7, X, zosah10, host) :-
    is_int_digit(X).

delta(zosah10, X, zosah13, host) :-
    is_int_char(X, ".").

% N.
delta(zosah13, X, zosah14, host) :-
    is_int_digit_between(X, 0, 1).

delta(zosah14, X, zosah15, host) :-
    is_int_digit(X).

delta(zosah15, X, zosah16, host) :-
    is_int_digit(X).

delta(zosah13, X, zosah17, host) :-
    is_int_char(X, "2").

delta(zosah17, X, zosah15, host) :-
    is_int_digit_between(X, 0, 4).

delta(zosah17, X, zosah18, host) :-
    is_int_char(X, "5").

delta(zosah18, X, zosah16, host) :-
    is_int_digit_between(X, 0, 5).

delta(zosah13, X, zosah15, host) :-
    is_int_digit(X).

delta(zosah13, X, zosah16, host) :-
    is_int_digit(X).

delta(zosah16, X, zosah19, host) :-
    is_int_char(X, ".").

% N
delta(zosah19, X, zosah20, host) :-
    is_int_digit_between(X, 0, 1).

delta(zosah20, X, zosah21, host) :-
    is_int_digit(X).

delta(zosah21, X, zosah22, host) :-
    is_int_digit(X).

delta(zosah19, X, zosah23, host) :-
    is_int_char(X, "2").

delta(zosah23, X, zosah21, host) :-
    is_int_digit_between(X, 0, 4).

delta(zosah23, X, zosah24, host) :-
    is_int_char(X, "5").

delta(zosah24, X, zosah22, host) :-
    is_int_digit_between(X, 0, 5).

delta(zosah19, X, zosah21, host) :-
    is_int_digit(X).

delta(zosah19, X, zosah22, host) :-
    is_int_digit(X).

% Id
delta(zosa1, X, zosah0, host) :-
    id_host(X).

delta(zosau1, X, zosah0, host) :-
    id_host(X).

delta(zosah0, X, zosah0, host) :-
    id_host(X).

delta(zosah0, X, zosah1, host) :-
    is_int_char(X, ".").

delta(zosah1, X, zosah0, host) :-
    id_host(X).

% Port
delta(zosah0, X, zosap0, other) :-
    is_int_char(X, ":").

delta(zosah22, X, zosap0, other) :-
    is_int_char(X, ":").

delta(zosap0, X, zosap1, port) :-
    is_int_digit(X).

delta(zosap1, X, zosap1, port) :-
    is_int_digit(X).

% Path
delta(zos0, X, zos1, other) :-
    is_int_char(X, "/").

delta(zosah0, X, zos1, other) :-
    is_int_char(X, "/").

delta(zosah22, X, zos1, other) :-
    is_int_char(X, "/").

delta(zosap1, X, zos1, other) :-
    is_int_char(X, "/").

% Id44
delta(zos1, X, zospath0, zpath44) :-
    is_int_alpha(X).

delta(zospath0, X, zospath0, zpath44) :-
    is_int_alphanum(X).

delta(zospath0, X, zospath0, zpath44) :-
    is_int_char(X, ".").

delta(zospath0, X, zospath1, zpath44) :-
    is_int_alphanum(X).

delta(zos1, X, zospath1, zpath44) :-
    is_int_alpha(X).

% Id8
delta(zospath1, X, zospath2, path) :-
    is_int_char(X, "(").

delta(zospath2, X, zospath3, zpath8) :-
    is_int_alpha(X).

delta(zospath3, X, zospath3, zpath8) :-
    is_int_alphanum(X).

delta(zospath3, X, zospath4, path) :-
    is_int_char(X, ")").

% Query
delta(zospath1, X, zosquery0, other) :-
    is_int_char(X, "?").

delta(zospath4, X, zosquery0, other) :-
    is_int_char(X, "?").

delta(zosquery0, X, zosquery1, query) :-
    id_query(X).

delta(zosquery1, X, zosquery1, query) :-
    id_query(X).

% Fragment
delta(zosquery1, X, zosfrag0, other) :-
    is_int_char(X, "#").

delta(zospath1, X, zosfrag0, other) :-
    is_int_char(X, "#").

delta(zospath4, X, zosfrag0, other) :-
    is_int_char(X, "#").

delta(zosfrag0, _, zosfrag1, fragment).

delta(zosfrag1, _, zosfrag1, fragment).


% URI1
% Scheme
delta(start, X, s0, scheme) :-
    identificatore(X).

delta(s0, X, s0, scheme) :-
    identificatore(X).

% :
delta(s0, X, q0, other) :-
    is_int_char(X, ":").

% Authorithy
delta(q0, X, a0, other) :-
    is_int_char(X, "/").

delta(a0, X, a1, other) :-
    is_int_char(X, "/").

% Userinfo
delta(a1, X, au0, user) :-
    identificatore(X).

delta(au0, X, au0, user) :-
    identificatore(X).

delta(au0, X, au1, other) :-
    is_int_char(X, "@").

% Host
% IP
% N.
delta(au1, X, ah2, host) :-
    is_int_digit_between(X, 0, 1).

delta(au1, X, ah3, host) :-
    is_int_char(X, "2").

delta(a1, X, ah2, host) :-
    is_int_digit_between(X, 0, 1).

delta(a1, X, ah3, host) :-
    is_int_char(X, "2").

delta(ah2, X, ah4, host) :-
    is_int_digit(X).

delta(ah4, X, ah5, host) :-
    is_int_digit(X).

delta(ah3, X, ah4, host) :-
    is_int_digit_between(X, 0, 4).

delta(ah3, X, ah6, host) :-
    is_int_char(X, "5").

delta(ah6, X, ah5, host) :-
    is_int_digit_between(X, 0, 5).

delta(au1, X, ah4, host) :-
    is_int_digit(X).

delta(au1, X, ah5, host) :-
    is_int_digit(X).

delta(a1, X, ah4, host) :-
    is_int_digit(X).

delta(a1, X, ah5, host) :-
    is_int_digit(X).

delta(ah5, X, ah7, host) :-
    is_int_char(X, ".").

% N.
delta(ah7, X, ah8, host) :-
    is_int_digit_between(X, 0, 1).

delta(ah8, X, ah9, host) :-
    is_int_digit(X).

delta(ah9, X, ah10, host) :-
    is_int_digit(X).

delta(ah7, X, ah11, host) :-
    is_int_char(X, "2").

delta(ah11, X, ah9, host) :-
    is_int_digit_between(X, 0, 4).

delta(ah11, X, ah12, host) :-
    is_int_char(X, "5").

delta(ah12, X, ah10, host) :-
    is_int_digit_between(X, 0, 5).

delta(ah7, X, ah9, host) :-
    is_int_digit(X).

delta(ah7, X, ah10, host) :-
    is_int_digit(X).

delta(ah10, X, ah13, host) :-
    is_int_char(X, ".").

% N.
delta(ah13, X, ah14, host) :-
    is_int_digit_between(X, 0, 1).

delta(ah14, X, ah15, host) :-
    is_int_digit(X).

delta(ah15, X, ah16, host) :-
    is_int_digit(X).

delta(ah13, X, ah17, host) :-
    is_int_char(X, "2").

delta(ah17, X, ah15, host) :-
    is_int_digit_between(X, 0, 4).

delta(ah17, X, ah18, host) :-
    is_int_char(X, "5").

delta(ah18, X, ah16, host) :-
    is_int_digit_between(X, 0, 5).

delta(ah13, X, ah15, host) :-
    is_int_digit(X).

delta(ah13, X, ah16, host) :-
    is_int_digit(X).

delta(ah16, X, ah19, host) :-
    is_int_char(X, ".").

% N
delta(ah19, X, ah20, host) :-
    is_int_digit_between(X, 0, 1).

delta(ah20, X, ah21, host) :-
    is_int_digit(X).

delta(ah21, X, ah22, host) :-
    is_int_digit(X).

delta(ah19, X, ah23, host) :-
    is_int_char(X, "2").

delta(ah23, X, ah21, host) :-
    is_int_digit_between(X, 0, 4).

delta(ah23, X, ah24, host) :-
    is_int_char(X, "5").

delta(ah24, X, ah22, host) :-
    is_int_digit_between(X, 0, 5).

delta(ah19, X, ah21, host) :-
    is_int_digit(X).

delta(ah19, X, ah22, host) :-
    is_int_digit(X).

% Id
delta(au1, X, ah0, host) :-
    id_host(X).

delta(a1, X, ah0, host) :-
    id_host(X).

delta(ah0, X, ah0, host) :-
    id_host(X).

delta(ah0, X, ah1, host) :-
    is_int_char(X, ".").

delta(ah1, X, ah0, host) :-
    id_host(X).

% Port
delta(ah0, X, ap0, other) :-
    is_int_char(X, ":").

delta(ah22, X, ap0, other) :-
    is_int_char(X, ":").

delta(ap0, X, ap1, port) :-
    is_int_digit(X).

delta(ap1, X, ap1, port) :-
    is_int_digit(X).

% /
delta(q0, X, q1, other) :-
    is_int_char(X, "/").

delta(ah0, X, q1, other) :-
    is_int_char(X, "/").

delta(ap1, X, q1, other) :-
    is_int_char(X, "/").

delta(ah22, X, q1, other) :-
    is_int_char(X, "/").

% Path
delta(q1, X, path0, path) :-
    identificatore(X).

delta(path0, X, path0, path) :-
    identificatore(X).

delta(path0, X, path1, path) :-
    is_int_char(X, "/").

delta(path1, X, path0, path) :-
    identificatore(X).

% Query
delta(q1, X, query0, other) :-
    is_int_char(X, "?").

delta(path0, X, query0, other) :-
    is_int_char(X, "?").

delta(query0, X, query1, query) :-
    id_query(X).

delta(query1, X, query1, query) :-
    id_query(X).

% Fragment
delta(q1, X, frag0, other) :-
    is_int_char(X, "#").

delta(path0, X, frag0, other) :-
    is_int_char(X, "#").

delta(query1, X, frag0, other) :-
    is_int_char(X, "#").

delta(frag0, _, frag1, fragment).

delta(frag1, _, frag1, fragment).


accept([I | Is], Q, [I | Ss], Us, Hs, Ps, Pts, Qs, Fs, Id44, Id8) :-
    delta(Q, I, N, scheme),
    accept(Is, N, Ss, Us, Hs, Ps, Pts, Qs, Fs, Id44, Id8).

accept([I | Is], Q, Ss, [I | Us], Hs, Ps, Pts, Qs, Fs, Id44, Id8) :-
    delta(Q, I, N, user),
    accept(Is, N, Ss, Us, Hs, Ps, Pts, Qs, Fs, Id44, Id8).

accept([I | Is], Q, Ss, Us, [I | Hs], Ps, Pts, Qs, Fs, Id44, Id8) :-
    delta(Q, I, N, host),
    accept(Is, N, Ss, Us, Hs, Ps, Pts, Qs, Fs, Id44, Id8).

accept([I | Is], Q, Ss, Us, Hs, [I | Ps], Pts, Qs, Fs, Id44, Id8) :-
    delta(Q, I, N, port),
    accept(Is, N, Ss, Us, Hs, Ps, Pts, Qs, Fs, Id44, Id8).

accept([I | Is], Q, Ss, Us, Hs, Ps, [I | Pts], Qs, Fs, Id44, Id8) :-
    delta(Q, I, N, path),
    accept(Is, N, Ss, Us, Hs, Ps, Pts, Qs, Fs, Id44, Id8).

accept([I | Is], Q, Ss, Us, Hs, Ps, [I | Pts], Qs, Fs, Id44, Id8) :-
    delta(Q, I, N, zpath44),
    NId44 is Id44 + 1,
    NId44 =< 44,
    accept(Is, N, Ss, Us, Hs, Ps, Pts, Qs, Fs, NId44, Id8).

accept([I | Is], Q, Ss, Us, Hs, Ps, [I | Pts], Qs, Fs, Id44, Id8) :-
    delta(Q, I, N, zpath8),
    NId8 is Id8 + 1,
    NId8 =< 8,
    accept(Is, N, Ss, Us, Hs, Ps, Pts, Qs, Fs, Id44, NId8).

accept([I | Is], Q, Ss, Us, Hs, Ps, Pts, [I | Qs], Fs, Id44, Id8) :-
    delta(Q, I, N, query),
    accept(Is, N, Ss, Us, Hs, Ps, Pts, Qs, Fs, Id44, Id8).

accept([I | Is], Q, Ss, Us, Hs, Ps, Pts, Qs, [I | Fs], Id44, Id8) :-
    delta(Q, I, N, fragment),
    accept(Is, N, Ss, Us, Hs, Ps, Pts, Qs, Fs, Id44, Id8).

accept([I | Is], Q, Ss, Us, Hs, Ps, Pts, Qs, Fs, Id44, Id8) :-
    delta(Q, I, N, other),
    accept(Is, N, Ss, Us, Hs, Ps, Pts, Qs, Fs, Id44, Id8).

accept([], Q, [], [], [], [], [], [], [], _, _) :- final(Q).


recognize(S, Scheme, Userinfo, Host, Port, Path, Query, Fragment) :-
    initial(Q),
    string_codes(S, L),
    accept(L, Q, Scheme, Userinfo, Host, Port, Path, Query, Fragment, 0, 0).


uri_parse(S, URI) :-
    recognize(S, Scheme, Userinfo, Host, Port, Path, Query, Fragment), !,
    convert(Scheme, AS),
    convert(Userinfo, AU),
    convert(Host, AH),
    convert_port(Port, AP),
    convert(Path, APt),
    convert(Query, AQ),
    convert(Fragment, AF),
    URI = uri(AS, AU, AH, AP, APt, AQ, AF).


uri_display(URI, Stream) :-
    write(Stream, URI).

uri_display(URI) :-
    write(URI).


identificatore(X) :-
    string_codes(S, [X]),
    S \= "/",
    S \= "?",
    S \= "#",
    S \= "@",
    S \= ":".


id_host(X) :-
    string_codes(S, [X]),
    S \= ".",
    S \= "/",
    S \= "?",
    S \= "#",
    S \= "@",
    S \= ":".


id_query(X) :-
    string_codes(S, [X]),
    S \= "#".


% is_int_char(X:Int, S:String)
%
% True when X is the integer representation of S

is_int_char(X, S) :-
    string_codes(S, [X]).


% is_not_int_char(X:Int, S:String)
%
% True when X is not the integer representation of S

is_not_int_char(X, S) :-
    string_codes(A, [X]),
    A \= S.


% is_int_digit(X:Int)
%
% True when X is the integer representation of a digit

is_int_digit(X) :-
    X >= 48,
    X =< 57.


% is_int_alpha(X:Int)
%
% True when X is the integer representation of a letter

is_int_alpha(X) :-
    X >= 65,
    X =< 122.


% is_int_alphanum(X:Int)
%
% True when X is the integer representation of
% either a letter or a digit

is_int_alphanum(X) :-
    is_int_digit(X).

is_int_alphanum(X) :-
    is_int_alpha(X).


% is_int_digit_between(X:Int, Y:Int, Z:Int)
%
% True when X is the integer representazion of a digit
% between LO, lower, and HO, higher, LO and HO included

is_int_digit_between(X, LO, HO) :-
    L is 48 + LO,
    H is 48 + HO,
    X >= L,
    X =< H.


% convert(X:List, Y:Atom)
%
% True when either Y is the Atom that represents the string codes
% in the list X or both X and Y are []

convert([], []) :- !.

convert(X, Y) :-
    X \= [], !,
    string_codes(S, X),
    atom_string(Y, S).


% convert_port(X:List, Y:Int)
%
% True when eother X is the empty list and port is 80
% or Y is the integer represented by X

convert_port([], 80) :- !.

convert_port(X, Y) :-
    string_codes(S, X),
    number_string(Y, S), !.
