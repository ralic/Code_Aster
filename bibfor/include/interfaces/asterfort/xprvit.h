        interface
          subroutine xprvit(noma,fiss,ndim,nvit,nbeta,lcmin,cnsvt,&
     &cnsvn,vpoint,cnsbl,cnsdis,disfr,cnsbet,listp,damax,locdom,rdimp,&
     &rdtor,delta,ucnslt,ucnsln)
            character(len=8) :: noma
            character(len=8) :: fiss
            integer :: ndim
            character(len=24) :: nvit
            character(len=24) :: nbeta
            real(kind=8) :: lcmin
            character(len=19) :: cnsvt
            character(len=19) :: cnsvn
            character(len=19) :: vpoint
            character(len=19) :: cnsbl
            character(len=19) :: cnsdis
            character(len=19) :: disfr
            character(len=19) :: cnsbet
            character(len=19) :: listp
            real(kind=8) :: damax
            logical :: locdom
            real(kind=8) :: rdimp
            real(kind=8) :: rdtor
            character(len=19) :: delta
            character(len=19) :: ucnslt
            character(len=19) :: ucnsln
          end subroutine xprvit
        end interface
