        interface
          subroutine nm1das(fami,kpg,ksp,e,syc,syt,etc,ett,cr,tmoins,&
     &tplus,icodma,sigm,deps,vim,sig,vip,dsdem,dsdep)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            real(kind=8) :: e
            real(kind=8) :: syc
            real(kind=8) :: syt
            real(kind=8) :: etc
            real(kind=8) :: ett
            real(kind=8) :: cr
            real(kind=8) :: tmoins
            real(kind=8) :: tplus
            integer :: icodma
            real(kind=8) :: sigm
            real(kind=8) :: deps
            real(kind=8) :: vim(4)
            real(kind=8) :: sig
            real(kind=8) :: vip(4)
            real(kind=8) :: dsdem
            real(kind=8) :: dsdep
          end subroutine nm1das
        end interface
