        interface
          subroutine calvci(nomci,nomnu,nbchci,lchci,inst,base)
            character(*) :: nomci
            character(*) :: nomnu
            integer :: nbchci
            character(*) :: lchci(*)
            real(kind=8) :: inst
            character(len=1) :: base
          end subroutine calvci
        end interface
