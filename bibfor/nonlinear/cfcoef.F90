subroutine cfcoef(ndimg, resoco, nbnom, posnsm, coefno,&
                  posnoe, norm, tau1, tau2, coef,&
                  cofx, cofy, nbddlt, ddl)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: ndimg
    character(len=24) :: resoco
    integer :: nbnom
    integer :: posnsm(9), posnoe
    real(kind=8) :: coefno(9)
    real(kind=8) :: norm(3), tau1(3), tau2(3)
    real(kind=8) :: coef(30), cofx(30), cofy(30)
    integer :: nbddlt
    integer :: ddl(30)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! CALCULE LES COEFFICIENTS DES RELATIONS LINEAIRES ET DONNE LES NUMEROS
!  DES DDL ASSOCIES
!
! ----------------------------------------------------------------------
!
!
! IN  NDIMG  : DIMENSION DE L'ESPACE (2 OU 3)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NBNOM  : NOMBRE DE NOEUDS MAITRES CONCERNES
! IN  POSNSM : INDICES DANS CONTNO DES NOEUDS MAITRES
! IN  POSNOE : INDICES DANS CONTNO DU NOEUD ESCLAVE
! IN  COEFNO : COEFFICIENTS DES FONCTIONS DE FORME APRES PROJECTION
!                SUR LA MAILLE MAITRE
! IN  NORM   : NORMALE
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! OUT COEF   : COEFFICIENTS LIES AU NOEUD ESCLAVE ET AUX NOEUDS MAITRES
!              (DIRECTION NORMALE)
! OUT COEFX  : COEFFICIENTS LIES AU NOEUD ESCLAVE ET AUX NOEUDS MAITRES
!              (PROJECTION SUR LA PREMIERE TANGENTE)
! OUT COEFY  : COEFFICIENTS LIES AU NOEUD ESCLAVE ET AUX NOEUDS MAITRES
!              (PROJECTION SUR LA SECONDE TANGENTE)
! OUT NBDDLT : NOMBRE DE DDLS CONCERNES (ESCLAVES + MAITRES)
! OUT DDL    : NUMEROS DES DDLS ESCLAVE ET MAITRES CONCERNES
!
!
!
!
    integer :: idim, ino, k
    integer :: jdecal, nbddlm, nbddle, jdecdl
    character(len=24) :: ddlco, nbddl
    integer :: jddl, jnbddl
    integer :: posnom
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    nbddl = resoco(1:14)//'.NBDDL'
    ddlco = resoco(1:14)//'.DDLCO'
    call jeveuo(nbddl, 'L', jnbddl)
    call jeveuo(ddlco, 'L', jddl)
!
! --- INITIALISATIONS
!
    do 1 k = 1, 30
        coef(k) = 0.d0
        cofx(k) = 0.d0
        cofy(k) = 0.d0
        ddl (k) = 0
 1  end do
!
! --- POUR LES NOEUDS ESCLAVES
!
    jdecdl = zi(jnbddl+posnoe-1)
    nbddle = zi(jnbddl+posnoe) - zi(jnbddl+posnoe-1)
!
    do 5 idim = 1, ndimg
        coef(idim) = 1.d0 * norm(idim)
        cofx(idim) = 1.d0 * tau1(idim)
        cofy(idim) = 1.d0 * tau2(idim)
        ddl(idim) = zi(jddl+jdecdl+idim-1)
 5  end do
    jdecal = nbddle
!
! --- POUR LES NOEUDS MAITRES
!
    do 100 ino = 1, nbnom
        posnom = posnsm(ino)
        jdecdl = zi(jnbddl+posnom-1)
        nbddlm = zi(jnbddl+posnom) - zi(jnbddl+posnom-1)
        do 85 idim = 1, nbddlm
            coef(jdecal+idim) = coefno(ino) * norm(idim)
            cofx(jdecal+idim) = coefno(ino) * tau1(idim)
            cofy(jdecal+idim) = coefno(ino) * tau2(idim)
            ddl(jdecal+idim) = zi(jddl+jdecdl+idim-1)
85      continue
        jdecal = jdecal + nbddlm
100  end do
!
! --- NOMBRE TOTAL DE DDL (NOEUD ESCLAVE+NOEUDS MAITRES)
!
    nbddlt = jdecal
!
    call jedema()
end subroutine
