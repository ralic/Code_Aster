        interface
          subroutine ctcrtb(nomtb,tych,resu,nkcha,typac,toucmp,nbcmp,&
     &nbval,nkcmp,ndim)
            character(len=8) :: nomtb
            character(len=4) :: tych
            character(len=8) :: resu
            character(len=24) :: nkcha
            character(len=8) :: typac
            logical :: toucmp
            integer :: nbcmp
            integer :: nbval
            character(len=24) :: nkcmp
            integer :: ndim
          end subroutine ctcrtb
        end interface
