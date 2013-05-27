        interface
          subroutine cnsred(cns1z,nbno,lino,nbcmp,licmp,base,cns2z)
            integer :: nbcmp
            integer :: nbno
            character(*) :: cns1z
            integer :: lino(nbno)
            character(*) :: licmp(nbcmp)
            character(*) :: base
            character(*) :: cns2z
          end subroutine cnsred
        end interface
