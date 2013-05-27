        interface
          subroutine cteltb(nbma,mesmai,noma,nbval,nkcha,nkcmp,toucmp,&
     &nbcmp,typac,ndim,nrval,resu,nomtb,nsymb,chpgs,tych,nival,niord)
            integer :: nbma
            character(len=24) :: mesmai
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
            character(len=19) :: chpgs
            character(len=4) :: tych
            character(len=24) :: nival
            character(len=24) :: niord
          end subroutine cteltb
        end interface
