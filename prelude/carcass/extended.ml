(** {1 Extended.$(NAME,capitalize)} *)

module type $(NAME,uppercase) = sig
end

(** Additional [$(NAME,capitalize)] functions.

    Typical usage:

    {v module $(NAME,capitalize) = struct include $(NAME,capitalize) include Extended.$(NAME,capitalize) ($(NAME,capitalize)) end
include $(NAME,capitalize).Ops v}
*)
module $(NAME,capitalize) ($(NAME,capitalize) : $(NAME,uppercase)) = struct
  open $(NAME,capitalize)

  (** {2 Ops} *)

  (** Infix and prefix operators, and other functions that should be imported. *)
  module Ops = struct
  end
end
