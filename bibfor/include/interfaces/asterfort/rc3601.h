        interface
          subroutine rc3601(ig,iocs,seisme,npass,ima,ipt,nbm,adrm,c,k,&
     &cara,nommat,snmax,samax,utot,sm,factus)
            integer :: ig
            integer :: iocs
            logical :: seisme
            integer :: npass
            integer :: ima
            integer :: ipt
            integer :: nbm
            integer :: adrm(*)
            real(kind=8) :: c(*)
            real(kind=8) :: k(*)
            real(kind=8) :: cara(*)
            character(len=8) :: nommat
            real(kind=8) :: snmax
            real(kind=8) :: samax
            real(kind=8) :: utot
            real(kind=8) :: sm
            real(kind=8) :: factus(*)
          end subroutine rc3601
        end interface
