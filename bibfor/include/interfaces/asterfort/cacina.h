        interface
          subroutine cacina(ndim,nno,npg,lgpg,axi,grand,compor,geomm,g&
     &,iw,vff,idff,fm,fma,depld,instm,instp,vim,rp,rpa,lambp)
            integer :: lgpg
            integer :: npg
            integer :: nno
            integer :: ndim
            logical :: axi
            logical :: grand
            character(len=16) :: compor(*)
            real(kind=8) :: geomm(3,nno)
            integer :: g
            integer :: iw
            real(kind=8) :: vff(nno,npg)
            integer :: idff
            real(kind=8) :: fm(3,3)
            real(kind=8) :: fma(3,3)
            real(kind=8) :: depld(81)
            real(kind=8) :: instm
            real(kind=8) :: instp
            real(kind=8) :: vim(lgpg)
            real(kind=8) :: rp(3,3)
            real(kind=8) :: rpa(3,3)
            real(kind=8) :: lambp(3,3)
          end subroutine cacina
        end interface
