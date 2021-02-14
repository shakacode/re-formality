module Test = {
  type t =
    | Placeholder;

  let fromUrl = (url: RescriptReactRouter.url) =>
    switch (url.hash) {
    | "Placeholder" => Placeholder
    | _ => failwith("Nothing here")
    };
};

[@react.component]
let make = () => {
  switch (RescriptReactRouter.useUrl()->Test.fromUrl) {
  | Placeholder => <Placeholder />
  };
};
