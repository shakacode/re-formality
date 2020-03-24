open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~target: Target.t, ~loc) =>
  switch (target) {
  | ReactDom => [%stri let _target = "ReactDom"]
  | ReactNative => [%stri let _target = "ReactNative"]
  };
