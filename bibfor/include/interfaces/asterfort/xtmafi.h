        interface
          subroutine xtmafi(noma,ndim,fiss,nfiss,lismai,mesmai,nbma)
            integer :: nfiss
            character(len=8) :: noma
            integer :: ndim
            character(len=8) :: fiss(nfiss)
            character(len=24) :: lismai
            character(len=24) :: mesmai
            integer :: nbma
          end subroutine xtmafi
        end interface
