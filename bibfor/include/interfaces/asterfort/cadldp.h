        interface
          subroutine cadldp(vp,sigeqe,nbmat,materf,parame,derive,sig3,&
     &eta,dg,detadg,dgdl,ddldsp)
            integer :: nbmat
            real(kind=8) :: vp(3)
            real(kind=8) :: sigeqe
            real(kind=8) :: materf(nbmat,2)
            real(kind=8) :: parame(4)
            real(kind=8) :: derive(5)
            real(kind=8) :: sig3
            real(kind=8) :: eta
            real(kind=8) :: dg
            real(kind=8) :: detadg
            real(kind=8) :: dgdl
            real(kind=8) :: ddldsp
          end subroutine cadldp
        end interface
