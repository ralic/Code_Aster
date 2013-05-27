        interface
          subroutine mpicm1(optmpi,typsca,nbv,bcrank,vi,vr,vc)
            character(*) :: optmpi
            character(*) :: typsca
            integer :: nbv
            integer :: bcrank
            integer :: vi(*)
            real(kind=8) :: vr(*)
            complex(kind=8) :: vc(*)
          end subroutine mpicm1
        end interface
