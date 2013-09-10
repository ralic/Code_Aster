subroutine prekpr(modmec, mtrmas, nbddl, numer, mailla,&
                  chamat, celem)
    implicit   none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: nbddl
    character(len=8) :: modmec, mtrmas, numer, mailla, chamat, celem
!-----------------------------------------------------------------------
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
!  BUT: RECHERCHE DES MATRICES ELEMENTAIRES POUR LA REPONSE EFFO_ELNO
!       DU CALCUL DYNAMIQUE ALEATOIRE
!
! IN  : MODMEC : CONCEPT MODE_MECA
! OUT : MTRMAS : MATRICE MASSE
! OUT : NBDDL  : NOMBRE DE DDLS DU PROBLEME
! OUT : NUMER  : CONCEPT NUMEROTATION
! OUT : MAILLA : CONCEPT MAILLAGE
! OUT : CHAMAT : CHAMP MATER
! OUT : CELEM  : CARA_ELEM
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
    integer :: ibid,ie
    character(len=8) :: k8b
!
!---------CONSTITUTION DE LA LISTE COMPLETE DES ET DDLS EN CAS D EFFORT
!---------RECUPERATION MODELE ET MAILLA
!
    call jemarq()
!


    call dismoi('F', 'REF_MASS_PREM', modmec, 'RESU_DYNA', ibid, mtrmas, ie)
    call dismoi('F', 'NB_EQUA', mtrmas, 'MATR_ASSE', nbddl,&
                k8b, ie)
    call dismoi('F', 'NOM_NUME_DDL', mtrmas, 'MATR_ASSE', ibid,&
                numer, ie)
    call dismoi('F', 'NOM_MAILLA', mtrmas, 'MATR_ASSE', ibid,&
                mailla, ie)
    call dismoi('F', 'CHAM_MATER', mtrmas, 'MATR_ASSE', ibid,&
                chamat, ie)
    call dismoi('F', 'CARA_ELEM', mtrmas, 'MATR_ASSE', ibid,&
                celem, ie)
!
    call jedema()
end subroutine
