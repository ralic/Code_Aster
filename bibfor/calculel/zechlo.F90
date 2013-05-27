subroutine zechlo(opt, te)
    implicit none
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    include 'jeveux.h'
    include 'asterc/indik8.h'
    include 'asterfort/assert.h'
    include 'asterfort/nbpara.h'
    include 'asterfort/nopara.h'
    integer :: opt, te
! ----------------------------------------------------------------------
!     BUT:
!      METTRE LES CHAMPS LOCAUX DE SORTIE A ZERO ENTRE 2 GRELS
!
! ----------------------------------------------------------------------
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    common /caii04/iachii,iachik,iachix
    common /caii07/iachoi,iachok
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: np, ipar
    integer :: lggrel, i, iachii, iachik, iachix, iachlo, iachoi, iachok
    integer :: iadsgd, iamloc, iaopds, iaopmo, iaopno, iaoppa
    integer :: iaoptt, ilmloc
    integer :: ilopmo, ilopno, iparg, lgco, nparin, npario
    character(len=1) :: typsca
    character(len=8) :: nompar
    integer :: debugr
!
! DEB-------------------------------------------------------------------
!
!     -- SI CALVOI==1 : IL N'Y A RIEN A FAIRE CAR LES CHAMPS LOCAUX
!        VIENNENT D'ETRE ALLOUES ET SONT DONC DEJA A ZERO.
    if (calvoi .eq. 1) goto 9999
!
!
    np = nbpara(opt,te,'OUT')
    do 40 ipar = 1, np
        nompar = nopara(opt,te,'OUT',ipar)
!
        iparg = indik8(zk8(iaoppa),nompar,1,npario)
        iachlo=zi(iawloc-1+3*(iparg-1)+1)
        if (iachlo .eq. -1) goto 40
!
        typsca = zk8(iawtyp-1+iparg) (1:1)
        call assert(typsca.eq.'R'.or.typsca.eq.'C'.or.typsca.eq.'I')
!
        lggrel=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+4)
        debugr=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+5)
        call assert(debugr.eq.1)
!
!
        if (typsca .eq. 'R') then
            do 10 i = 1, lggrel
                zr(iachlo-1+i) = 0.0d0
10          continue
        else if (typsca.eq.'C') then
            do 20 i = 1, lggrel
                zc(iachlo-1+i) = dcmplx(0.d0,0.d0)
20          continue
        else if (typsca.eq.'I') then
            do 30 i = 1, lggrel
                zi(iachlo-1+i) = 0
30          continue
        endif
!
40  end do
!
9999  continue
end subroutine
