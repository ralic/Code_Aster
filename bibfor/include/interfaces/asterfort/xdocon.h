        interface
          subroutine xdocon(algocr,algofr,cface,contac,coefcp,coeffp,&
     &coefcr,coeffr,elc,fpg,ifiss,ivff,jcface,jdonco,jlonch,mu,nspfis,&
     &ncompd,ndim,nface,ninter,nnof,nomte,npgf,nptf,rela)
            integer :: algocr
            integer :: algofr
            integer :: cface(5,3)
            integer :: contac
            real(kind=8) :: coefcp
            real(kind=8) :: coeffp
            real(kind=8) :: coefcr
            real(kind=8) :: coeffr
            character(len=8) :: elc
            character(len=8) :: fpg
            integer :: ifiss
            integer :: ivff
            integer :: jcface
            integer :: jdonco
            integer :: jlonch
            real(kind=8) :: mu
            integer :: nspfis
            integer :: ncompd
            integer :: ndim
            integer :: nface
            integer :: ninter
            integer :: nnof
            character(len=16) :: nomte
            integer :: npgf
            integer :: nptf
            real(kind=8) :: rela
          end subroutine xdocon
        end interface
