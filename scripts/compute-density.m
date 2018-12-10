#!/usr/bin/wolframscript -script
(* Computes the relative density of G_n in T. Requires the *)
(* G_0,...,G_n grammars to be available in respective i.in files. *)
(* Densities are computed using the simplified Szego Lemma. *)
(* Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl> *)

T[z_] = (1-Sqrt[1-4*z])/(2*z) - 1;
S[z_] = ((1-Sqrt[1-4*z])/(2*z)) * (z/(1-z));
B[z_] = (z/(1-z)); (* the symbol N conflicts with the built-in N function *)

n = ToExpression[$ScriptCommandLine[[2]]];
Do[Subscript[G,i][z_] = ToExpression[ReadString[StringJoin[ToString[i],".in"]]], {i,0,n,1}];

P[z_] = Subscript[G,n][z] /. z -> (z/4);
R[z_] = P[z] /. z -> (1-z^2);
H[z_] = R[z] /. Sqrt[z^2] -> z;
d = Simplify[(H'[0])/(-2)];

Print[N[d]]
