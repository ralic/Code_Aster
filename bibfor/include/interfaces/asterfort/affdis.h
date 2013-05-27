        interface
          subroutine affdis(ndim,irep,eta,car,val,jdc,jdv,ivr,iv,kma,&
     &ncmp,ntp,jdcinf,jdvinf,isym,ifm)
            integer :: ndim
            integer :: irep
            real(kind=8) :: eta
            character(*) :: car
            real(kind=8) :: val(*)
            integer :: jdc(3)
            integer :: jdv(3)
            integer :: ivr(*)
            integer :: iv
            character(len=1) :: kma(3)
            integer :: ncmp
            integer :: ntp
            integer :: jdcinf
            integer :: jdvinf
            integer :: isym
            integer :: ifm
          end subroutine affdis
        end interface
