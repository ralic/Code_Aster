        interface
          subroutine vafcar(tpgz,mclfz,nmobjz,npo,ndi,nco,nca,nba,nma,&
     &ngb,nmb,nutyel,ntyele,car,ncar,ivr,kioc,ier)
            character(*) :: tpgz
            character(*) :: mclfz
            character(*) :: nmobjz
            integer :: npo
            integer :: ndi
            integer :: nco
            integer :: nca
            integer :: nba
            integer :: nma
            integer :: ngb
            integer :: nmb
            integer :: nutyel
            integer :: ntyele(*)
            character(*) :: car(*)
            integer :: ncar
            integer :: ivr(*)
            character(len=6) :: kioc
            integer :: ier
          end subroutine vafcar
        end interface
