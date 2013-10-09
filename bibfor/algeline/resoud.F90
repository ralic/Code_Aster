subroutine resoud(matass, matpre, solveu, chcine, nsecm,&
                  chsecm, chsolu, base, rsolu, csolu,&
                  criter, prepos, istop, iret)
    implicit none
# include "jeveux.h"
# include "asterfort/assert.h"
# include "asterfort/copisd.h"
# include "asterfort/dismoi.h"
# include "asterfort/elg_calc_solu.h"
# include "asterfort/elg_kellag.h"
# include "asterfort/elg_resoud.h"
# include "asterfort/jedema.h"
# include "asterfort/jedetr.h"
# include "asterfort/jelira.h"
# include "asterfort/jemarq.h"
# include "asterfort/jeveuo.h"
# include "asterfort/resou1.h"
# include "asterfort/uttcpu.h"
!-----------------------------------------------------------------------
!
    character(len=*) :: matass, matpre, solveu, chcine
    integer :: nsecm
    character(len=*) :: chsecm, chsolu, base
    real(kind=8) :: rsolu(*)
    complex(kind=8) :: csolu(*)
    character(len=*) :: criter
    logical :: prepos
    integer :: istop, iret
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
!-----------------------------------------------------------------------
! BUT : RESOUDRE UN SYSTEME LINEAIRE D'EQUATIONS (REEL OU COMPLEXE)
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!
! REMARQUES : ON PEUT APPELER RESOUD DE 2 FACONS
!   1) AVEC NSECM = 0 + CHSECM, CHSOLU, BASE
!   2) AVEC NSECM > 0 + RSOLU (OU CSOLU) + (CHSECM=CHSOLU=' ')
!
! IN/JXIN  K19 MATASS : MATR_ASSE PREMIER MEMBRE DU SYSTEME LINEAIRE
! IN/JXIN  K19 MATPRE : MATR_ASSE DE PRECONDITIONNEMENT
!                       POUR SOLVEUR ITERATIF GCPC (OU ' ' SINON)
! IN/JXIN  K19 SOLVEU : SD_SOLVEUR (OU ' ')
!                       SI SOLVEU=' ' ON PREND LE SOLVEUR DE MATASS
! IN/JXIN  K*  CHCINE : CHAMP ASSOCIE AUX CHARGES CINEMATIQUES (OU ' ')
! IN       I   NSECM  : / 0 => ON UTILISE CHSECM, CHSOLU, BASE
!                       / N => ON UTILISE RSOLU (OU CSOLU)
!                         N : NOMBRE DE SECONDS MEMBRES
! IN/JXIN  K*  CHSECM : CHAMP SECOND MEMBRE DU SYSTEME LINEAIRE
! IN/JXOUT K*  CHSOLU : CHAMP SOLUTION DU SYSTEME LINEAIRE
! IN       K*  BASE   : BASE SUR LAQUELLE ON CREE CHSOLU
! IN/OUT   R   RSOLU  : TABLEAU (*,NSECM)
!           EN ENTREE : VECTEUR DE REELS CONTENANT LES SECONDS MEMBRES
!           EN SORTIE : VECTEUR DE REELS CONTENANT LES SOLUTIONS
! IN/OUT   C   CSOLU  : TABLEAU (*,NSECM)
!                       IDEM RSOLU POUR LES COMPLEXES
! IN/JXOUT K*  CRITER : SD_CRITER (CRITERES DE CONVERGENCE)
!                       POUR SOLVEUR ITERATIF GCPC (OU ' ' SINON)
! IN       L   PREPOS : / .TRUE.  => ON FAIT LES PRE ET POST-TRAITEMENTS
!                                    DU SMB ET DE LA SOLUTION
!                       / .FALSE. => ON NE FAIT AUCUN TRAITEMENT
!                                    (EN MODAL PAR EXEMPLE)
! IN       I   ISTOP  : COMPORTEMENT EN CAS D'ERREUR (CE PARAMETRE N'A
!                       D'UTILITE QUE POUR UN SOLVEUR ITERATIF)
!                       / 0     : ON S'ARRETE EN <F>
!                       / 2     : ON CONTINUE SANS MESSAGE D'ERREUR
!                       / -9999 : ON PREND LA VALEUR DEFINIE DANS LA
!                                 SD_SOLVEUR POUR STOP_SINGULIER
! OUT      I   IRET   : CODE RETOUR
!                       / 0 : OK (PAR DEFAUT POUR SOLVEURS DIRECTS)
!                       / 1 : ECHEC (NOMBRE MAX. D'ITERATIONS ATTEINT)
!-----------------------------------------------------------------------
! cette routine est une surcouche de la routine resou1.
! elle est necessaire pour traiter le cas elim_lagr='oui'
! ----------------------------------------------------------------------
    character(len=19) :: matas1, solve1
    character(len=3) :: kellag
! ----------------------------------------------------------------------
!
    call jemarq()
    call uttcpu('CPU.RESO.1', 'DEBUT', ' ')
    call uttcpu('CPU.RESO.5', 'DEBUT', ' ')
    matas1=matass
    solve1=solveu
!
!
!   1. CALCUL DE KELLAG :
!   -------------------------------------
    call elg_kellag(matas1, solve1, kellag)
!
!
!   2. SI ELIM_LAGR /= 'OUI', ON APPELLE SIMPLEMENT RESOU1 :
!   --------------------------------------------------------
    if (.not.(kellag.eq.'OUI')) then
        call resou1(matas1, matpre, solve1, chcine, nsecm,&
                    chsecm, chsolu, base, rsolu, csolu,&
                    criter, prepos, istop, iret)
    else
        call elg_resoud(matas1, chcine, nsecm, chsecm, chsolu,&
                        base, rsolu, csolu, criter, prepos,&
                        istop, iret)
    endif
!
!
    call uttcpu('CPU.RESO.1', 'FIN', ' ')
    call uttcpu('CPU.RESO.5', 'FIN', ' ')
    call jedema()
end subroutine
