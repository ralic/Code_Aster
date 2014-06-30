subroutine affono(valr, valk, desc, prnm, nbcomp,&
                  fonree, nomn, ino, nsurch, forimp,&
                  valfor, valfof, motcle, verif, nbec)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
#include "asterfort/exisdg.h"
#include "asterfort/utmess.h"
    integer :: prnm(1), nbcomp, desc, ino, nsurch, forimp(nbcomp)
    real(kind=8) :: valr(1), valfor(nbcomp)
    logical(kind=1) :: verif
    character(len=4) :: fonree
    character(len=8) :: valk(1), nomn, valfof(nbcomp)
    character(len=16) :: motcle(nbcomp), valkk(2)
!
! BUT : * DETECTER ET PRENDRE EN COMPTE LES SURCHARGES DANS FORCE_NODALE
!        1 REMPLIR LES TABLEAUX DESCRIPTEUR DE FORCES IMPOSEES
!        2 COMPTER LE NOMBRE DE SURCHARGES DE FORCE_NODALE
!          POUR PERMETTE L'ALLOCATION DES CARTES A LA BONNE DIMENSION
!        3 EMET UN MESSAGE D'ALARME EN CAS DE SURCHARGE
!
! ARGUMENTS D'ENTREE:
!
!   PRNM   : DESCRIPTEUR GRANDEUR SUR LE NOEUD INO
!   NBCOMP : NOMBRE DE DDLS DANS FORCE_NODALE
!   FONREE : AFFE_CHAR_XXXX OU AFFE_CHAR_XXXX_F
!   NOMN   : NOM DU NOEUD INO OU EST EFFECTUEE L'AFFECTATION
!   INO    : NUMERO DU NOEUD OU EST EFFECTUEE L'AFFECTATION
!   FORIMP : INDICATEUR DE PRESENCE OU ABSENCE DE FORCE SUR CHAQUE DDL
!   VALFOR : VALEURS AFFECTEES SUR CHAQUE DDL (FONREE = 'REEL')
!   VALFOF : VALEURS AFFECTEES SUR CHAQUE DDL (FONREE = 'FONC')
!   MOTCLE : TABLEAU DES NOMS DES DDLS DANS FORCE_NODALE
!   VERIF  : BOOLEEN ( TRUE ---> VERIFICATION SI LE DDL AFFECTE
!                                EST PRESENT SUR LE NOEUD
!                      FALSE --> ON PASSE ET ON INCREMENTE LA SURCHARGE)
!   NBEC   : NOMBRE D'ENTIERS CODES REPRESENTANT LA GRANDEUR
!
! ARGUMENTS D'ENTREE MODIFIES:
!
!      VALR  : VALEURS DES DDLS DE FORCES  (FONREE = 'REEL')
!      VALK  : VALEURS DES DDLS DE FORCES  (FONREE = 'FONC')
!      DESC  : TABLEAU CONTENANT LE DESCRIPTEUR DES AFFECTATIONS
!              (CODES A PARTIR DU PREMIER BIT)
!     NSURCH : COMPTEUR DU NOMBRE DE SURCHARGES DANS FORCE_NODALE
!
!
!****************************************************************
!-----------------------------------------------------------------------
    integer :: iec, indigd, j, nbec, nsurc0
!-----------------------------------------------------------------------
    indigd = 0
    do 10 iec = 1, nbec
        if (prnm(iec) .ne. 0) then
            indigd = 1
            goto 20
        endif
10  end do
    if (indigd .eq. 0) goto 9999
20  continue
    nsurc0 = nsurch
    do 30 j = 1, nbcomp
        if (forimp(j) .ne. 0) then
            if (iand(desc,2**(j-1)) .eq. 0) then
!  VERIFICATION SUR LES 6 PREMIERS DDL : FX FY FZ MX MY MZ
                if (.not.exisdg(prnm,j) .and. j .le. 6) then
                    if (.not.verif) then
                        if (nsurc0 .eq. nsurch) nsurch = nsurch + 1
                    else
                        valkk (1) = motcle(j)
                        valkk (2) = nomn
                        call utmess('F', 'MODELISA8_27', nk=2, valk=valkk)
                    endif
                else
                    desc = ior(desc,2**(j-1))
                endif
            else
                if (nsurc0 .eq. nsurch) nsurch = nsurch + 1
                valkk (1) = motcle(j)
                valkk (2) = nomn
                call utmess('I', 'MODELISA8_28', nk=2, valk=valkk)
            endif
            if (fonree .eq. 'REEL') then
                valr(nbcomp*(ino-1)+j) = valfor(j)
            else if (fonree.eq.'FONC') then
                valk(nbcomp*(ino-1)+j) = valfof(j)
            endif
        endif
30  end do
!
9999  continue
end subroutine
