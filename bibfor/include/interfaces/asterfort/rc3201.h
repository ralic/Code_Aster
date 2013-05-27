        interface
          subroutine rc3201(lpmpb,lsn,lsnet,lfatig,lrocht,lieu,ig,iocs&
     &,seisme,npass,mater,snmax,snemax,spmax,kemax,spmecm,spthem,samax,&
     &utot,sm,sigpm,resuas,resuss,resuca,resucs,factus,pmmax,pbmax,&
     &pmbmax)
            logical :: lpmpb
            logical :: lsn
            logical :: lsnet
            logical :: lfatig
            logical :: lrocht
            character(len=4) :: lieu
            integer :: ig
            integer :: iocs
            logical :: seisme
            integer :: npass
            character(len=8) :: mater
            real(kind=8) :: snmax
            real(kind=8) :: snemax
            real(kind=8) :: spmax
            real(kind=8) :: kemax
            real(kind=8) :: spmecm
            real(kind=8) :: spthem
            real(kind=8) :: samax
            real(kind=8) :: utot
            real(kind=8) :: sm
            real(kind=8) :: sigpm
            real(kind=8) :: resuas(*)
            real(kind=8) :: resuss(*)
            real(kind=8) :: resuca(*)
            real(kind=8) :: resucs(*)
            real(kind=8) :: factus(*)
            real(kind=8) :: pmmax
            real(kind=8) :: pbmax
            real(kind=8) :: pmbmax
          end subroutine rc3201
        end interface
