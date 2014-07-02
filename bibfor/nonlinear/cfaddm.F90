subroutine cfaddm(resoco, lctfd, lctf3d, posnoe, iliai,&
                  ndimg, nbnom, posnsm, coefno, tau1,&
                  tau2, norm, jeu, coornp)
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
#include "asterfort/cfcoef.h"
#include "asterfort/cfcoem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    integer :: posnoe, iliai
    integer :: nbnom, ndimg
    integer :: posnsm(*)
    real(kind=8) :: coefno(*)
    real(kind=8) :: jeu, coornp(3)
    real(kind=8) :: norm(3), tau1(3), tau2(3)
    aster_logical :: lctfd, lctf3d
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! ON AJOUTE UNE LIAISON MAITRE/ESCLAVE OU NODALE
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  LCTFD  : FROTTEMENT
! IN  LCTF3D : FROTTEMENT EN 3D
! IN  POSNOE : INDICE DANS CONTNO DU NOEUD ESCLAVE
! IN  ILIAI  : INDICE DE LA LIAISON COURANTE
! IN  NDIMG  : DIMENSION DE L'ESPACE
! IN  NBNOM  : NOMBRE DE NOEUDS MAITRES CONCERNES (MAX: 9)
! IN  POSNSM : INDICES DANS CONTNO DES NOEUDS MAITRES
! IN  COEFNO : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
!               MAITRES
! IN  TAU1   : TANGENTE LOCALE DIRECTION 1
! IN  TAU2   : TANGENTE LOCALE DIRECTION 2
! IN  NORM   : NORMALE LOCALE
! IN  JEU    : JEU DANS LA DIRECTION DE LA NORMALE CHOISIE (PM.NORM)
! IN  COORNP : COORDONNNES DE LA PROJECTION DU NOEUD ESCLAVE
!
!
!
!
    character(len=24) :: tangco, approj
    integer :: jtango, jappro
    character(len=24) :: jeuite
    integer :: jjeuit
    real(kind=8) :: coef(30), cofx(30), cofy(30)
    integer :: nbddlt
    integer :: ddl(30)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    tangco = resoco(1:14)//'.TANGCO'
    jeuite = resoco(1:14)//'.JEUITE'
    approj = resoco(1:14)//'.APPROJ'
    call jeveuo(tangco, 'E', jtango)
    call jeveuo(jeuite, 'E', jjeuit)
    call jeveuo(approj, 'E', jappro)
!
! --- CALCUL DES COEFFICIENTS DES RELATIONS LINEAIRES
!
    call cfcoef(ndimg, resoco, nbnom, posnsm, coefno,&
                posnoe, norm, tau1, tau2, coef,&
                cofx, cofy, nbddlt, ddl)
!
! --- MISE A JOUR DU JEU POUR LE CONTACT
!
    zr(jjeuit+3*(iliai-1)+1-1) = jeu
!
! --- INITIALISATIONS JEUX FROTTEMENT EN 3D
!
    if (lctf3d) then
        zr(jjeuit+3*(iliai-1)+2-1) = 0.d0
        zr(jjeuit+3*(iliai-1)+3-1) = 0.d0
    endif
!
! --- SAUVEGARDE TANGENTES
!
    zr(jtango+6*(iliai-1)+1-1) = tau1(1)
    zr(jtango+6*(iliai-1)+2-1) = tau1(2)
    zr(jtango+6*(iliai-1)+3-1) = tau1(3)
    zr(jtango+6*(iliai-1)+4-1) = tau2(1)
    zr(jtango+6*(iliai-1)+5-1) = tau2(2)
    zr(jtango+6*(iliai-1)+6-1) = tau2(3)
!
! --- SAUVEGARDE COORDONNEES DE LA PROJECTION
!
    zr(jappro+3*(iliai-1)+1-1) = coornp(1)
    zr(jappro+3*(iliai-1)+2-1) = coornp(2)
    zr(jappro+3*(iliai-1)+3-1) = coornp(3)
!
! --- COEFFICIENTS RELATIONS LINEAIRES APPARIEMENT
!
    call cfcoem(resoco, lctfd, lctf3d, posnoe, iliai,&
                nbddlt, nbnom, posnsm, ddl, coef,&
                cofx, cofy)
!
    call jedema()
end subroutine
