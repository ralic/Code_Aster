        interface
          subroutine ngpipe(typilo,npg,neps,nddl,b,li2ldc,typmod,mat,&
     &compor,lgpg,ddlm,sigm,vim,ddld,ddl0,ddl1,tau,etamin,etamax,copilo)
            integer :: lgpg
            integer :: nddl
            integer :: neps
            integer :: npg
            character(len=16) :: typilo
            real(kind=8) :: b(neps,npg,nddl)
            real(kind=8) :: li2ldc(0:neps-1)
            character(len=8) :: typmod(*)
            integer :: mat
            character(len=16) :: compor(4)
            real(kind=8) :: ddlm(nddl)
            real(kind=8) :: sigm(0:neps*npg-1)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: ddld(nddl)
            real(kind=8) :: ddl0(nddl)
            real(kind=8) :: ddl1(nddl)
            real(kind=8) :: tau
            real(kind=8) :: etamin
            real(kind=8) :: etamax
            real(kind=8) :: copilo(5,npg)
          end subroutine ngpipe
        end interface
