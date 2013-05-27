        interface
          subroutine lrmmdi(fid,nomamd,typgeo,nomtyp,nnotyp,nmatyp,&
     &nbnoeu,nbmail,nbnoma,descfi,adapma)
            integer :: fid
            character(*) :: nomamd
            integer :: typgeo(*)
            character(len=8) :: nomtyp(*)
            integer :: nnotyp(*)
            integer :: nmatyp(*)
            integer :: nbnoeu
            integer :: nbmail
            integer :: nbnoma
            character(*) :: descfi
            character(*) :: adapma
          end subroutine lrmmdi
        end interface
