subroutine mmimp4(ifm, noma, nummae, iptm, indcoi,&
                  indcon, indfri, indfrn, lfrot, lvites,&
                  lgliss, jeu, jeuvit, lambdc)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
    integer :: ifm
    integer :: nummae
    character(len=8) :: noma
    integer :: iptm
    integer :: indcoi, indcon
    integer :: indfri, indfrn
    aster_logical :: lfrot, lvites, lgliss
    real(kind=8) :: jeu, jeuvit, lambdc
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE - IMPRESSIONS)
!
! AFFICHAGE ALGORITHME CONTRAINTES ACTIVES
!
! ----------------------------------------------------------------------
!
!
! IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMMAE : NUMERO ABSOLU DE LA MAILLE ESCLAVE
! IN  IPTM   : NUMERO DU POINT DE CONTACT DANS LA MAILLE ESCLAVE
! IN  INDCOI : INDICATEUR DE CONTACT INITIAL
!              - INDCOI = 0: PAS DE CONTACT
!              - INDCOI = 1: CONTACT
! IN  INDCON : INDICATEUR DE CONTACT FINAL
!              - INDCON = 0: PAS DE CONTACT
!              - INDCON = 1: CONTACT
! IN  INDFRI : INDICATEUR DE FROTTEMENT INITIAL
!              - INDFRI = 0 SI ADHERENT
!              - INDFRI = 1 SI GLISSANT
! IN  INDFRN : INDICATEUR DE FROTTEMENT FINAL
!              - INDFRN = 0 SI ADHERENT
!              - INDFRN = 1 SI GLISSANT
! IN  LVITES : .TRUE. SI FORMULATION EN VITESSE
! IN  LGLISS : .TRUE. SI CONTACT GLISSIERE
! IN  LFROT  : .TRUE. SI FROTTEMENT
! IN  JEU    : JEU TOTAL (Y COMPRIS DIST_*)
! IN  JEUVIT : GAP VITESSE
! IN  LAMBDC : LAGRANGE DE CONTACT (PRESSION DE CONTACT)
!
! ----------------------------------------------------------------------
!
    character(len=8) :: nomesc
    character(len=16) :: statut
!
! ----------------------------------------------------------------------
!
!
! --- REPERAGE MAILLE ESCLAVE
!
    call jenuno(jexnum(noma//'.NOMMAI', nummae), nomesc)
    write(ifm,1000) nomesc
    write(ifm,2000) iptm
    1000 format (' <CONTACT>     * LA MAILLE ESCLAVE ',a8)
    2000 format (' <CONTACT>     ** DONT LE POINT DE CONTACT ',i3)
!
! --- PROPRIETES (FORMULATION)
!
    if (lvites) then
        write(ifm,1001)
    else
        write(ifm,1002)
    endif
    1001 format (' <CONTACT>        (FORMULATION VITESSE)')
    1002 format (' <CONTACT>        (FORMULATION DEPLACEMENT)')
!
! --- PROPRIETES (GLISSIERE)
!
    if (lgliss) then
        write(ifm,3001)
    endif
    3001 format (' <CONTACT>        (GLISSIERE)')
!
! --- ETAT DE CONTACT INITIAL
!
    if (indcoi .eq. 0) then
        statut = ' PAS EN CONTACT '
    else
        statut = ' EN CONTACT     '
    endif
    write(ifm,5000) statut
    5000 format (' <CONTACT>         -> ETAT DE CONTACT    INITIAL:',a16)
!
! --- ETAT DE FROTTEMENT INITIAL
!
    if (lfrot) then
        if (indfri .eq. 0) then
            statut = ' ADHERENT '
        else
            statut = ' GLISSANT '
        endif
        write(ifm,5001) statut
    endif
    5001 format (' <CONTACT>         -> ETAT DE FROTTEMENT INITIAL:',a16)
!
! --- PROPRIETES (JEUX)
!
    if (lvites) then
        write(ifm,6001) jeu,jeuvit,lambdc
    else
        write(ifm,6000) jeu,lambdc
    endif
    6000 format (' <CONTACT>         <> JEU:',e10.3,&
     &        ' - LAGS_C :',e10.3)
!
    6001 format (' <CONTACT>         <> JEU:',e10.3,&
     &        ' - SAUT VITESSE:',e10.3,&
     &        ' - LAGS_C :',e10.3)
!
! --- ETAT DE CONTACT FINAL
!
    if (indcon .eq. 0) then
        statut = ' PAS EN CONTACT '
    else
        statut = ' EN CONTACT     '
    endif
    write(ifm,7000) statut
    7000 format (' <CONTACT>         -> ETAT DE CONTACT    FINAL  :',a16)
!
! --- ETAT DE FROTTEMENT FINAL
!
    if (lfrot) then
        if (indfrn .eq. 0) then
            statut = ' ADHERENT '
        else
            statut = ' GLISSANT '
        endif
        write(ifm,7001) statut
    endif
    7001 format (' <CONTACT>         -> ETAT DE FROTTEMENT FINAL  :',a16)
!
end subroutine
