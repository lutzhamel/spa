% imp-equiv-bool.pl

:- ['imp.pl'].

:- >>> 'prove that true ~ not(false)'.

% show that
% (forall s)(exists B0,B1)
%      [s::true ==> B0 ^ s::not(false) ==> B1 ^ B0=B1]

:- show 
      state(s):: true ==> B0,
      state(s):: not(false) ==> B1,
      B0=B1.

:- qed.