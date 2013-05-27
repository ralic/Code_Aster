        interface
          subroutine ctdata(mesnoe,mesmai,nkcha,tych,toucmp,nkcmp,&
     &nbcmp,ndim,chpgs,noma,nbno,nbma,nbval,tsca)
            character(len=24) :: mesnoe
            character(len=24) :: mesmai
            character(len=24) :: nkcha
            character(len=4) :: tych
            logical :: toucmp
            character(len=24) :: nkcmp
            integer :: nbcmp
            integer :: ndim
            character(len=19) :: chpgs
            character(len=8) :: noma
            integer :: nbno
            integer :: nbma
            integer :: nbval
            character(len=1) :: tsca
          end subroutine ctdata
        end interface
