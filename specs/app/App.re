module Test = {
  type t =
    | Placeholder;

  let fromUrl = (url: ReasonReactRouter.url) =>
    switch (url.hash) {
    | "Placeholder" => Placeholder
    | _ => failwith("Nothing here")
    };
};

[@react.component]
let make = () => {
  switch (ReasonReactRouter.useUrl()->Test.fromUrl) {
  | Placeholder => <Placeholder />
  };
};
