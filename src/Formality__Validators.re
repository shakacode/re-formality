module type Config = {type t;};

module Make = (Config: Config) =>
  Map.Make(
    {
      type t = Config.t;
      let compare = Formality__Utils.comparator;
    }
  );
