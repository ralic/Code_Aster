subroutine mmopti(loptin, resoco, seuili, ctcini, lgliss,&
                  iptc, epsint, jeusgn)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: resoco
    integer :: ctcini, iptc
    real(kind=8) :: seuili, epsint, jeusgn
    logical :: loptin, lgliss
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT)
!
! REMPLIT LA SD APPARIEMENT POUR LES OPTIONS
!
! ----------------------------------------------------------------------
!
!
! IN  LOPTIN : VAUT .TRUE. SI ACTIVATION DES OPTIONS *_INIT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  SEUILI : VALEUR DU SEUIL_INIT
! IN  CTCINI : VAUT .TRUE. SI CONTACT_INIT
! IN  LGLISS : SI CONTACT GLISSIERE
! IN  IPTC   : NUMERO DE LA LIAISON DE CONTACT
! IN  EPSINT : TOLERANCE POUR CONTACT_INIT
! IN  JEUSGN : JEU SIGNE
!
! ----------------------------------------------------------------------
!
    integer :: ztabf
    character(len=24) :: tabfin
    integer :: jtabf
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONTACT
!
    tabfin = resoco(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'E', jtabf)
    ztabf = cfmmvd('ZTABF')
!
! --- SEUIL_INIT
!
    if (loptin) then
        zr(jtabf+ztabf*(iptc-1)+16) = seuili
    endif
!
! --- CONTACT_INIT
!
    if (loptin) then
        if (ctcini .eq. 2) then
! ------- SEULEMENT POUR LES POINTS INTERPENETRES
            if (jeusgn .le. epsint) then
                zr(jtabf+ztabf*(iptc-1)+22) = 1.d0
                if (lgliss) then
                    zr(jtabf+ztabf*(iptc-1)+17) = 1.d0
                endif
            endif
        else if (ctcini.eq.1) then
! ------- POUR TOUS LES POINTS
            zr(jtabf+ztabf*(iptc-1)+22) = 1.d0
            if (lgliss) then
                zr(jtabf+ztabf*(iptc-1)+17) = 1.d0
            endif
        else if (ctcini.eq.0) then
! ------- PAS DE CONTACT INITIAL
            zr(jtabf+ztabf*(iptc-1)+22) = 0.d0
        else
            call assert(.false.)
        endif
    endif
!
! --- NOEUDS EXCLUS: PAS EN CONTACT
!
    if (zr(jtabf+ztabf*(iptc-1)+18) .eq. 1.d0) then
        zr(jtabf+ztabf*(iptc-1)+22) = 0.d0
    endif
!
    call jedema()
!
end subroutine
