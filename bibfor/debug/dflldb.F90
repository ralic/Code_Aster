subroutine dflldb(sdlist, ifm)
!
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dflld2.h"
#include "asterfort/dfllvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: sdlist
    integer :: ifm
!
! ----------------------------------------------------------------------
!
! OPERATEUR DEFI_LIST_INST
!
! IMPRESSION DEBUG
!
! ----------------------------------------------------------------------
!
! IN  SDLIST : NOM DE LA SD RESULTAT
! IN  IFM    : UNITE LOGIQUE AFFICHAGE
!
!
!
!
    character(len=24) :: lisifr
    integer :: jlinr
    integer :: iechec, nechec, nbinst, nadapt
    integer :: nbpamx
    real(kind=8) :: dtmin, pasmin, pasmax
    integer :: leevr, leevk, lesur
    character(len=24) :: lisevr, lisevk, lisesu
    integer :: jeevr, jeevk, jesur
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- TAILLE DES VECTEURS
!
    leevr = dfllvd('LEEVR')
    leevk = dfllvd('LEEVK')
    lesur = dfllvd('LESUR')
!
! --- ACCES SDS
!
    lisifr = sdlist(1:8)//'.LIST.INFOR'
    call jeveuo(lisifr, 'L', jlinr)
!
! --- LONGUEURS
!
    nechec = nint(zr(jlinr-1 + 9))
    nbinst = nint(zr(jlinr-1 + 8))
    nadapt = nint(zr(jlinr-1 + 10))
!
! --- GESTION DE LA LISTE D'INSTANTS
!
    if (zr(jlinr-1+1) .eq. 1.d0) then
        write(ifm,*) '<DEFILISTINST> GESTION MANUELLE '//&
     &               'DE LA LISTE D''INSTANTS'
    else if (zr(jlinr-1+1).eq.2.d0) then
        write(ifm,*) '<DEFILISTINST> GESTION AUTOMATIQUE '//&
     &               'DE LA LISTE D''INSTANTS'
    else
        call assert(.false.)
    endif
    dtmin = zr(jlinr-1+5)
    write(ifm,*) '<DEFILISTINST> ... LA LISTE CONTIENT ',nbinst,&
     &             ' INSTANTS ET LE PAS MINIMUM VAUT ',dtmin
!
! --- PARA. GESTION AUTO PAS DE TEMPS
!
    if (zr(jlinr-1+1) .eq. 2.d0) then
        pasmin = zr(jlinr-1+2)
        pasmax = zr(jlinr-1+3)
        nbpamx = nint(zr(jlinr-1+4))
        write(ifm,*) '<DEFILISTINST> PARAMETRES DE LA GESTION  '//&
     &               'AUTOMATIQUE DE LA LISTE D''INSTANTS'
        write(ifm,*) '<DEFILISTINST> ... PAS MINI   : ',pasmin
        write(ifm,*) '<DEFILISTINST> ... PAS MAXI   : ',pasmax
        write(ifm,*) '<DEFILISTINST> ... NB_PAS_MAXI: ',nbpamx
    endif
!
! --- ECHEC
!
    if (nechec .gt. 0) then
        lisevr = sdlist(1:8)//'.ECHE.EVENR'
        lisevk = sdlist(1:8)//'.ECHE.EVENK'
        lisesu = sdlist(1:8)//'.ECHE.SUBDR'
        call jeveuo(lisevr, 'L', jeevr)
        call jeveuo(lisevk, 'L', jeevk)
        call jeveuo(lisesu, 'L', jesur)
        write(ifm,*) '<DEFILISTINST> GESTION DES EVENEMENTS (',&
        nechec,' EVENEMENTS)'
        do 10 iechec = 1, nechec
            write(ifm,*) '<DEFILISTINST> ... EVENEMENT : ',iechec
            if (zr(jeevr-1+leevr*(iechec-1)+1) .eq. 0.d0) then
                write(ifm,*) '<DEFILISTINST> ...... ERRE'
            else if (zr(jeevr-1+leevr*(iechec-1)+1).eq.1.d0) then
                write(ifm,*) '<DEFILISTINST> ...... DELTA_GRANDEUR'
                write(ifm,*) '<DEFILISTINST> ......... CHAMP      :',&
     &         zk16(jeevk-1+leevk*(iechec-1)+1)
                write(ifm,*) '<DEFILISTINST> ......... COMPOSANTE :',&
                zk16(jeevk-1+leevk*(iechec-1)+2)
                write(ifm,*) '<DEFILISTINST> ......... COMPARATEUR:',&
                zk16(jeevk-1+leevk*(iechec-1)+3)
            else if (zr(jeevr-1+leevr*(iechec-1)+1).eq.2.d0) then
                write(ifm,*) '<DEFILISTINST> ...... COLLISION'
            else if (zr(jeevr-1+leevr*(iechec-1)+1).eq.3.d0) then
                write(ifm,*) '<DEFILISTINST> ...... INTERPENETRATION'
                write(ifm,*) '<DEFILISTINST> ......... PENE_MAXI  :',&
     &         zr(jeevr-1+leevr*(iechec-1)+6)
            else if (zr(jeevr-1+leevr*(iechec-1)+1).eq.4.d0) then
                write(ifm,*) '<DEFILISTINST> ...... DIVE_RESI'
            else if (zr(jeevr-1+leevr*(iechec-1)+1).eq.5.d0) then
                write(ifm,*) '<DEFILISTINST> ...... INSTABILITE'
            else
                call assert(.false.)
            endif
!
! ------- ACTION
!
            if (zr(jeevr-1+leevr*(iechec-1)+2) .eq. 0.d0) then
                write(ifm,*) '<DEFILISTINST> ...... ARRET DU CALCUL'
!
            else if (zr(jeevr-1+leevr*(iechec-1)+2).eq.1.d0) then
                write(ifm,*) '<DEFILISTINST> ...... DECOUPE DU PAS'//&
                ' DE TEMPS'
                call dflld2(sdlist, ifm, iechec)
!
            else if (zr(jeevr-1+leevr*(iechec-1)+2).eq.2.d0) then
                write(ifm,*) '<DEFILISTINST> ...... AUGMENTATION'//&
                ' DU NOMBRE D''ITERATIONS DE NEWTON'
                write(ifm,*) '<DEFILISTINST> ......... EN'//&
     &                   ' PERMETTANT',&
     &                   nint(zr(jesur-1+lesur*(iechec-1)+7)),&
     &                   ' % D''ITERATIONS EN PLUS'
!
                if (zr(jesur-1+lesur*(iechec-1)+1) .eq. 0.d0) then
                    write(ifm,*) '<DEFILISTINST> ....... SANS '//&
                    ' PERMETTRE UN DECOUPAGE EN CAS D''ECHEC'
!
                else if (zr(jesur-1+lesur*(iechec-1)+1).eq.1.d0) then
                    write(ifm,*) '<DEFILISTINST> ....... EN'//&
                    ' PERMETTANT UN DECOUPAGE EN CAS D''ECHEC'
                    call dflld2(sdlist, ifm, iechec)
!
                else if (zr(jesur-1+lesur*(iechec-1)+1).eq.2.d0) then
                    write(ifm,*) '<DEFILISTINST> ....... EN'//&
                    ' PERMETTANT UN DECOUPAGE EN CAS D''ECHEC'
                    call dflld2(sdlist, ifm, iechec)
                else
                    call assert(.false.)
                endif
!
!
            else if (zr(jeevr-1+leevr*(iechec-1)+2).eq.3.d0) then
                write(ifm,*) '<DEFILISTINST> ...... CHANGEMENT'//&
                ' DE LA SOLUTION DE PILOTAGE'
                if (zr(jesur-1+lesur*(iechec-1)+1) .eq. 0.d0) then
                    write(ifm,*) '<DEFILISTINST> ....... SANS '//&
                    ' PERMETTRE UN DECOUPAGE EN CAS D''ECHEC'
!
!
                else if (zr(jesur-1+lesur*(iechec-1)+1).eq.1.d0) then
                    write(ifm,*) '<DEFILISTINST> ....... EN'//&
                    ' PERMETTANT UN DECOUPAGE EN CAS D''ECHEC'
                    call dflld2(sdlist, ifm, iechec)
                endif
            else if (zr(jeevr-1+leevr*(iechec-1)+2).eq.4.d0) then
                write(ifm,*) '<DEFILISTINST> ...... ADAPTATION'//&
                ' DU COEFFICIENT DE PENALISATION'
                write(ifm,*) '<DEFILISTINST> ......... EN'//&
     &                   ' PERMETTANT UN COEF. MAXI DE: ',&
     &                   zr(jesur-1+lesur*(iechec-1)+8)
!
            else if (zr(jeevr-1+leevr*(iechec-1)+2).eq.5.d0) then
                write(ifm,*) '<DEFILISTINST> ...... ON CONTINUE LE CALCUL'
!
            else
                call assert(.false.)
            endif
10      continue
!
    endif
!
! --- ADAPTATION
!
    if (nadapt .gt. 0) then
        write(ifm,*) '<DEFILISTINST> SCHEMAS D''ADAPTATION DU'//&
     &               ' PAS DE TEMPS  (',&
     &               nadapt,' ADAPTATIONS)'
    endif
!
    call jedema()
end subroutine
