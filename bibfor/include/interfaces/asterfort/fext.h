        interface
          subroutine fext(t,neq,nvect,liad,lifo,f)
            real(kind=8) :: t
            integer :: neq
            integer :: nvect
            integer :: liad(*)
            character(len=24) :: lifo(*)
            real(kind=8) :: f(*)
          end subroutine fext
        end interface
