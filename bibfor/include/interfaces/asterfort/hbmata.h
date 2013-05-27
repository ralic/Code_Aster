        interface
          subroutine hbmata(se,dg,etap,i1e,sigeqe,vp,vecp,parame,&
     &derive,sig3,detadg,dgdl,nbmat,materf,dsidep)
            integer :: nbmat
            real(kind=8) :: se(6)
            real(kind=8) :: dg
            real(kind=8) :: etap
            real(kind=8) :: i1e
            real(kind=8) :: sigeqe
            real(kind=8) :: vp(3)
            real(kind=8) :: vecp(3,3)
            real(kind=8) :: parame(4)
            real(kind=8) :: derive(5)
            real(kind=8) :: sig3
            real(kind=8) :: detadg
            real(kind=8) :: dgdl
            real(kind=8) :: materf(nbmat,2)
            real(kind=8) :: dsidep(6,6)
          end subroutine hbmata
        end interface
