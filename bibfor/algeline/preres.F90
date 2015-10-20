subroutine preres(solveu, base, iret, matpre, matass,&
                  npvneg, istop)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jemarq.h"
#include "asterfort/prere3.h"
#include "asterfort/prere2.h"
#include "asterfort/xfem_ksolv.h"
#include "asterfort/matr_asse_syme.h"
#include "asterfort/uttcpu.h"
!-----------------------------------------------------------------------
    integer :: npvneg, istop, iret
    character(len=1) :: base
    character(len=*) :: matass, matpre, solveu
!-----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! BUT : FACTORISER UNE MATR_ASSE (LDLT/MULT_FRONT/MUMPS)
!       OU FABRIQUER UNE MATRICE DE PRECONDITIONNEMENT (GCPC,PETSC)
!
! SOLVEU (K19) IN : OBJET SOLVEUR
! BASE (K1)    IN : BASE SUR LAQUELLE ON CREE LA MATRICE FACTORISEE
!                  (OU LA MATRICE DE PRECONDITIONNEMENT)
! IRET (I)     OUT : CODE_RETOUR :
!             /0 -> OK (PAR DEFAUT AVEC GCPC/PETSC)
!             /2 -> LA FACTORISATION N'A PAS PU SE FAIRE
!                   JUSQU'AU BOUT.
!             /1 -> LA FACTORISATION EST ALLEE AU BOUT
!                   MAIS ON A PERDU BEAUCOUP DE DECIMALES
!             /3 -> LA FACTORISATION EST ALLEE AU BOUT
!                   MAIS ON NE SAIT PAS DIRE SI ON A PERDU DES DECIMALES
!
! MATPRE(K19) IN/JXVAR : MATRICE DE PRECONDITIONNEMENT (SI GCPC)
! MATASS(K19) IN/JXVAR : MATRICE A FACTORISER OU A PRECONDITIONNER
! NPVNEG (I) OUT : NBRE DE TERMES DIAGONAUX NEGATIFS DE LA FACTORISEE
!          CE NBRE N'EST LICITE QUE SI LA MATRICE EST REELLE SYMETRIQUE
!          ET N'EST FOURNI QUE PAR UN SOLVEUR DIRECT: LDLT, MF OU MUMPS
! ISTOP (I)  IN: COMPORTEMENT EN CAS DE DETECTION DE SINGULARITE. CE
!                PARAMETRE N'A D'UTILITE QU'AVEC UN SOLVEUR DIRECT
!                  /0 -> SI IRET>0 : ERREUR <F>
!                  /1 -> SI IRET=1 : ALARME <A>
!                        SI IRET=2 : ERREUR <F>
!                  /2 -> LE PROGRAMME NE S'ARRETE PAS
!                        ET N'IMPRIME AUCUN MESSAGE.
!                 /-9999 -> ON PREND LA VALEUR PREVUE DS LA SD_SOLVEUR
!                        POUR STOP_SINGULIER (VALEUR 0 OU 1 SEULEMENT)
!                 /AUTRE --> ASSERT
!-----------------------------------------------------------------------
! Cette routine est une surcouche de la routine prere1.
! elle est necessaire pour traiter le cas ELIM_LAGR='OUI'
!----------------------------------------------------------------------
    character(len=3) :: kxfem
    character(len=19) :: matas1
!----------------------------------------------------------------------
    call jemarq()
    call uttcpu('CPU.RESO.1', 'DEBUT', ' ')
    call uttcpu('CPU.RESO.4', 'DEBUT', ' ')
    matas1=matass
    ASSERT(solveu.ne.' ')


!    VERIFICATION SI XFEM :
!   -------------------------------------
    call xfem_ksolv(solveu, kxfem)
    if ( kxfem .eq. 'OUI') then
       call prere3(solveu, base, iret, matpre, matass,&
                  npvneg, istop)
    else
       call prere2(solveu, base, iret, matpre, matass,&
                  npvneg, istop)
    endif

    call uttcpu('CPU.RESO.1', 'FIN', ' ')
    call uttcpu('CPU.RESO.4', 'FIN', ' ')
    call jedema()
end subroutine
