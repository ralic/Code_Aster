        interface
          subroutine ctnotb(nbno,mesnoe,noma,nbval,nkcha,nkcmp,toucmp,&
     &nbcmp,typac,ndim,nrval,resu,nomtb,nsymb,nival,niord)
            integer :: nbno
            character(len=24) :: mesnoe
            character(len=8) :: noma
            integer :: nbval
            character(len=24) :: nkcha
            character(len=24) :: nkcmp
            logical :: toucmp
            integer :: nbcmp
            character(len=8) :: typac
            integer :: ndim
            character(len=24) :: nrval
            character(len=8) :: resu
            character(len=8) :: nomtb
            character(len=16) :: nsymb
            character(len=24) :: nival
            character(len=24) :: niord
          end subroutine ctnotb
        end interface
