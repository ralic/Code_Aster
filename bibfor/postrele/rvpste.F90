subroutine rvpste(dim, lieu, ssch19, nomsd, typaff)
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/i3crdm.h'
    include 'asterfort/i3drdm.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rvechc.h'
    include 'asterfort/rveche.h'
    include 'asterfort/rvechm.h'
    include 'asterfort/rvechn.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: nomsd, lieu
    character(len=19) :: ssch19
    character(len=2) :: dim
    character(len=1) :: typaff
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATION EXTRACTION DU POST-TRAITEMENT DE L' OCCURENCE COURANTE
!     ------------------------------------------------------------------
! IN  DIM    : K : '1D' OU '2D' OU '3D'(RELIQUAR --> MESSAGE)
! IN  LIEU   : K : OJB S V K24 NOMS DE LIEU DE POST-TRAITEMENT
! IN  SSCH19 : K : SS_CHAM_19 DE L' EXTRACTION DES CMP NECESSAIRES
! OUT NOMSD  : K : OJB S V K24  NOM DE SD D' EVALUATION SUR LES LIEUX
!     ------------------------------------------------------------------
!     LIEU DE POST-TRAITEMENT ::= RECORD
!        .ABSC : XD V R
!        .REFE : S E K8
!         CHOIX DOCU(.REFE) PARMI
!         -----             -----
!          'SGTD' : DESC = (XA,YA,XB,YB)       REFE =  NOM_COURBE
!          'SGT3' : DESC = (XA,YA,ZA,XB,YB,ZB) REFE =  NOM_COURBE
!          'ARCC' : DESC = (XC,YC,R,A1,A2)     REFE =  NOM_COURBE
!          'CHMM' : DESC = L_NOMS_NOEUDS       REFE =  NOM_COURBE
!          'LSTN' : DESC = L_NOMS_NOEUDS       REFE =  NOM_MAILLAGE
!         FIN_CHOIX
!         ---------
!
!     SD_EVAL
!        . UNE SD_EVAL PAR LIEU DE POST
!        . TYPE : SOUS_CHAM_GD
!        . LE LIEU JOUE LE ROLE D' UN MAILLAGE
!             NOEUD  : POINT DE POST
!             MAILLE : TYPE SEG2 DANS LES CAS 'SGTD','ARCC',CHMM'
!                      TYPE POI1 DANS LES CAS 'LSTN'
!
!    REPRESENTATION SOUS_CHAM_GD :
!        'CHNO' : STANDARD
!        'CHLM' : SEG2 --> NB_CMP PAR POINT, 2 POINTS
!                 POI1 --> NB_CMP PAR POINT, N POINTS
!                          N EST LE NOMBRE DE MAILLES SIGNIFICATIVES
!                          DU MAILLAGE INITIAL
!     ------------------------------------------------------------------
!
!
    integer :: alieu, ibid, l, nbl, anomsd, i, anbndf, aclocf, adescm
    character(len=24) :: nrefe, descm
    character(len=19) :: sdlieu, sdeval
    character(len=4) :: docu
    character(len=1) :: k1bid
!
!==================== CORPS DE LA ROUTINE =============================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    descm = '&&RVPSTE.PTR.DESC.TYP.MA'
    call jelira(lieu, 'LONMAX', nbl, k1bid)
    call jeveuo(lieu, 'L', alieu)
    call wkvect(nomsd, 'V V K24', nbl, anomsd)
    call i3crdm(descm)
    call jeveuo(descm, 'L', adescm)
    call wkvect('&&RVPSTE.NB.ND.FACE.TYPE', 'V V I', 18, anbndf)
    call wkvect('&&RVPSTE.CNC.LOC.FA.TYPE', 'V V I', 72, aclocf)
    do 5, i = 1, 3, 1
    ibid = zi(adescm + i-1)
    do 6, l = 1, 6, 1
    zi(anbndf + 6*(i-1) + l-1) = zi(ibid + l+1)
 6  continue
    do 7, l = 1, 24, 1
    zi(aclocf + 24*(i-1) + l-1) = zi(ibid + l+7)
 7  continue
    5 end do
    do 10, l = 1, nbl, 1
    sdlieu = zk24(alieu + l-1)(1:19)
    sdeval = sdlieu
    sdeval(1:8) = '&&RVPSTE'
    zk24(anomsd + l-1) = sdeval//'     '
    nrefe = sdlieu//'.REFE'
    call jelira(nrefe, 'DOCU', ibid, docu)
    if (docu .eq. 'SGTD') then
        call rvechc(dim, ssch19, sdlieu, sdeval, zi(anbndf),&
                    zi(aclocf))
    else if (docu .eq. 'ARCC') then
        call rvechc(dim, ssch19, sdlieu, sdeval, zi(anbndf),&
                    zi(aclocf))
    else if (docu .eq. 'SGT3') then
        call rvechc(dim, ssch19, sdlieu, sdeval, zi(anbndf),&
                    zi(aclocf))
    else if (docu .eq. 'CHMM') then
        call rvechm(ssch19, sdlieu, sdeval)
    else if (docu .eq. 'LSTN') then
        if (typaff .eq. 'N') then
            call rvechn(ssch19, sdlieu, sdeval)
        else
            call rveche(ssch19, sdlieu, sdeval)
        endif
    else
!           AUTRE LIEU DE POST-TRAITEMENT
    endif
    10 end do
    call jedetr('&&RVPSTE.NB.ND.FACE.TYPE')
    call jedetr('&&RVPSTE.CNC.LOC.FA.TYPE')
    call i3drdm(descm)
    call jedema()
end subroutine
