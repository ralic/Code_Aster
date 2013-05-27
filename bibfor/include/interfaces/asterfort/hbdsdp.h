        interface
          subroutine hbdsdp(se,dg,etap,sigeqe,vp,parame,derive,nbmat,&
     &materf,sig3,detadg,dgdl,dsdsip)
            integer :: nbmat
            real(kind=8) :: se(6)
            real(kind=8) :: dg
            real(kind=8) :: etap
            real(kind=8) :: sigeqe
            real(kind=8) :: vp(3)
            real(kind=8) :: parame(4)
            real(kind=8) :: derive(5)
            real(kind=8) :: materf(nbmat,2)
            real(kind=8) :: sig3
            real(kind=8) :: detadg
            real(kind=8) :: dgdl
            real(kind=8) :: dsdsip(6)
          end subroutine hbdsdp
        end interface
