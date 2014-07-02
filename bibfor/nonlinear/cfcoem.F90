subroutine cfcoem(resoco, lctfd, lctf3d, posnoe, iliai,&
                  nbddlt, nbnom, posnsm, ddl, coef,&
                  cofx, cofy)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    integer :: posnoe
    integer :: iliai
    integer :: nbddlt
    integer :: nbnom
    integer :: posnsm(9)
    integer :: ddl(30)
    real(kind=8) :: coef(30), cofx(30), cofy(30)
    aster_logical :: lctfd, lctf3d
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT - MAIT/ESCL)
!
! COEFFICIENTS RELATIONS LINEAIRES APPARIEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  LCTFD  : FROTTEMENT
! IN  LCTF3D : FROTTEMENT EN 3D
! IN  POSNOE : INDICE DANS CONTNO DU NOEUD ESCLAVE
! IN  ILIAI  : INDICE DE LA LIAISON COURANTE
! IN  NBDDLT : NOMBRE DE DDL NOEUD ESCLAVE+NOEUDS MAITRES
! IN  NBNOM  : NOMBRE DE NOEUDS MAITRES CONCERNES (MAX: 9)
! IN  POSNSM : INDICES DANS CONTNO DES NOEUDS MAITRES
! IN  DDL    : NUMEROS DES DDLS ESCLAVE ET MAITRES CONCERNES
! IN  COEF   : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
! IN  COFX   : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
!                POUR LA PREMIERE DIRECTION TANGENTE
! IN  COFY   : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
!                POUR LA SECONDE DIRECTION TANGENTE
!
!
!
!
    integer :: nbddle, nbddlm, nesmax
    integer :: jdecal, jdecdl, k, ino, posnom
    integer :: japcoe, japcof
    character(len=24) :: apcoef, apcofr
    character(len=24) :: nbddl, apddl
    integer :: jnbddl, japddl
    character(len=24) :: appoin
    integer :: japptr
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    nbddl = resoco(1:14)//'.NBDDL'
    apddl = resoco(1:14)//'.APDDL'
    call jeveuo(nbddl, 'L', jnbddl)
    call jeveuo(apddl, 'E', japddl)
!
    apcoef = resoco(1:14)//'.APCOEF'
    apcofr = resoco(1:14)//'.APCOFR'
    appoin = resoco(1:14)//'.APPOIN'
!
    call jeveuo(appoin, 'E', japptr)
    call jeveuo(apcoef, 'E', japcoe)
    if (lctfd) then
        call jeveuo(apcofr, 'E', japcof)
    endif
!
    nesmax = cfdisd(resoco,'NESMAX')
!
! --- NOMBRE DE DDL POUR LE NOEUD ESCLAVE
!
    nbddle = zi(jnbddl+posnoe) - zi(jnbddl+posnoe-1)
!
! --- NOMBRE TOTAL RELATIONS LINEAIRES
!
    zi(japptr+iliai) = zi(japptr+iliai-1) + nbddlt
!
! --- RELATION DE CONTACT POUR LE NOEUD ESCLAVE
!
    jdecal = zi(japptr+iliai-1)
    do 5 k = 1, nbddle
        zr(japcoe+jdecal+k-1) = coef(k)
        zi(japddl+jdecal+k-1) = ddl(k)
  5 end do
!
! --- RELATION DE FROTTEMENT POUR LE NOEUD ESCLAVE
!
    if (lctfd) then
        do 10 k = 1, nbddle
            zr(japcof+jdecal+k-1) = cofx(k)
            if (lctf3d) then
                zr(japcof+jdecal+30*nesmax+k-1) = cofy(k)
            endif
 10     continue
    endif
!
! --- RELATION DE CONTACT POUR LE NOEUD MAITRE
!
    jdecal = jdecal + nbddle
    jdecdl = nbddle
    do 50 ino = 1, nbnom
        posnom = posnsm(ino)
        nbddlm = zi(jnbddl+posnom) - zi(jnbddl+posnom-1)
        do 40 k = 1, nbddlm
            zr(japcoe+jdecal+k-1) = coef(jdecdl+k)
            zi(japddl+jdecal+k-1) = ddl(jdecdl+k)
 40     continue
!
! --- RELATION DE FROTTEMENT POUR LE NOEUD MAITRE
!
        if (lctfd) then
            do 41 k = 1, nbddlm
                zr(japcof+jdecal+k-1) = cofx(jdecdl+k)
                if (lctf3d) then
                    zr(japcof+jdecal+30*nesmax+k-1) = cofy(jdecdl+k)
                endif
 41         continue
        endif
        jdecal = jdecal + nbddlm
        jdecdl = jdecdl + nbddlm
 50 end do
!
    call jedema()
end subroutine
