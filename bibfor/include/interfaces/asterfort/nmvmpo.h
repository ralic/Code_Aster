        interface
          subroutine nmvmpo(fami,npg,option,nomte,nc,xl,icodma,sect,&
     &carcri,compor,u,du,contm,hoel,hota,d1b,work,rg0,contp,fl,klv)
            integer :: nc
            character(*) :: fami
            integer :: npg
            character(*) :: option
            character(*) :: nomte
            real(kind=8) :: xl
            integer :: icodma
            real(kind=8) :: sect(*)
            real(kind=8) :: carcri(*)
            character(len=16) :: compor(*)
            real(kind=8) :: u(2*nc)
            real(kind=8) :: du(2*nc)
            real(kind=8) :: contm(3*nc)
            real(kind=8) :: hoel(nc,nc)
            real(kind=8) :: hota(nc,nc)
            real(kind=8) :: d1b(nc,2*nc)
            real(kind=8) :: work(nc,2*nc)
            real(kind=8) :: rg0(2*nc,2*nc)
            real(kind=8) :: contp(3*nc)
            real(kind=8) :: fl(2*nc)
            real(kind=8) :: klv(*)
          end subroutine nmvmpo
        end interface
