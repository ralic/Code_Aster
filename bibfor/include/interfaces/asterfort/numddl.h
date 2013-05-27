        interface
          subroutine numddl(nu,base,nbmat,tlimat,method)
            integer :: nbmat
            character(*) :: nu
            character(len=2) :: base
            character(*) :: tlimat(*)
            character(*) :: method
          end subroutine numddl
        end interface
