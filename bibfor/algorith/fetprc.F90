subroutine fetprc(nbsd, nbi, vd1, vd2, vdo,&
                  matas, vddl, preco, infofe, irex,&
                  ifiv, nbproc, rang, k24ir2)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  PRECONDITIONNEMENT AU SENS FETI
!                          PRECO='SANS'  : VO = V1
!                          PRECO='LUMPE' : VO = ML-1 * V1
!
!      IN   NBSD: IN   : NOMBRE DE SOUS-DOMAINES
!      IN    NBI: IN   : NOMBRE DE NOEUDS D'INTERFACE
!      IN    VD1: VR8  : VECTEUR V DE TAILLE NBI (INPUT)
!      IN    VD2: VR8  : VECTEUR AUXILIAIRE DE TAILLE NBI
!      OUT   VDO: VR8  : VECTEUR OUTPUT DE TAILLE NBI
!      IN  MATAS: CH19 : NOM DE LA MATR_ASSE GLOBALE
!      IN   VDDL: VIN  : VECTEUR DES NBRES DE DDLS DES SOUS-DOMAINES
!      IN  PRECO: K24 : TYPE DE PRECONDITIONNEMENT
!     IN IREX/IFIV: IN : ADRESSE DU VECTEUR AUXILAIRE EVITANT DES APPELS
!                        JEVEUX.
!     IN RANG  : IN  : RANG DU PROCESSEUR
!     IN NBPROC: IN  : NOMBRE DE PROCESSEURS
!     IN K24IR2: K24 : NOM DE L'OBJET JEVEUX VDO POUR LE PARALLELISME
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/fetmpi.h"
#include "asterfort/fetrex.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    integer :: nbsd, nbi, vddl(nbsd), irex, ifiv, nbproc, rang
    real(kind=8) :: vd1(nbi), vdo(nbi), vd2(nbi)
    character(len=19) :: matas
    character(len=24) :: preco, infofe, k24ir2
!
!
! DECLARATION VARIABLES LOCALES
    integer :: idd, nbddl, idd1, jxsol, j, lmat, jxsol2, k, ifm, ilimpi, nivmpi
    integer :: ibid
    real(kind=8) :: rbid
    character(len=24) :: k24b
    logical :: lpara
!
! INITS DIVERSES
    if (nbproc .eq. 1) then
        lpara=.false.
    else
        lpara=.true.
    endif
    if (infofe(10:10) .eq. 'T') then
        nivmpi=2
    else
        nivmpi=1
    endif
!
! ROUTINE AVEC MOINS DE MONITORING, APPELS JEVEUX.. CAR APPELLEE TRES
! SOUVENT
    ifm=zi(ifiv)
    if (preco(1:4) .eq. 'SANS') then
! PAS DE PRECONDITIONNEMENT: ZR(IR1)=ZR(IRG) (VD0=VD1)
        call dcopy(nbi, vd1, 1, vdo, 1)
! MONITORING
        if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETPRC', rang, '> SANS PRECONDITIONNEMENT'
!
    else if (preco(1:5).eq.'LUMPE') then
! PRECONDITIONNEMENT LUMPE: ZR(IR1)=ML-1*ZR(IRG) (VD0=ML-1*VD1)
! MONITORING
        if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETPRC', rang, '> PRECONDITIONNEMENT LUMPE'
!
! INIT. VECTEUR SOLUTION ET AUX
        do 10 j = 1, nbi
            vdo(j)=0.d0
10      continue
! ADRESSE JEVEUX OBJET FETI & MPI
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
        do 40 idd = 1, nbsd
! LE SOUS-DOMAINE IDD EST IL CONCERNE PAR LE PROCESSUS ACTUEL ?
            if (zi(ilimpi+idd) .eq. 1) then
                idd1=idd-1
! DESCRIPTEUR DE LA MATRICE DU SOUS-DOMAINE
                k=ifiv+2+idd1*5
                lmat=zi(k)
! NBRE DE DDL DU SOUS-DOMAINE IDD
                nbddl=vddl(idd)
! VECTEURS AUXILIAIRES DE TAILLE NDDL(SOUS_DOMAINE_IDD)
                jxsol=zi(k+3)
                jxsol2=zi(k+4)
!
! EXTRACTION DU VECTEUR V AU SOUS-DOMAINE IDD: (RIDD)T * V
                call fetrex(2, idd, nbi, vd1, nbddl,&
                            zr(jxsol), irex)
!
!
! PRODUIT:  (KI) * (RIDD)T * V
                call mrmult('ZERO', lmat, zr(jxsol), zr(jxsol2), 1,&
                            .false.)
!
! RESTRICTION DU SOUS-DOMAINE IDD SUR L'INTERFACE: (RIDD) * ...
                call fetrex(1, idd, nbddl, zr(jxsol2), nbi,&
                            vd2, irex)
! CUMUL DANS LE VECTEUR VDO=SOMME(I=1,NBSD)(RI * ((KI)+ * RIT * V))
                call daxpy(nbi, 1.d0, vd2, 1, vdo,&
                           1)
            endif
!========================================
! FIN BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
40      continue
    else
        ASSERT(.false.)
    endif
    if ((preco(1:4).eq.'SANS') .or. (preco(1:5).eq.'LUMPE')) then
! COLLECTE DU RESIDU INITIAL POUR LE PROCESSUS MAITRE
        if (lpara) call fetmpi(7, nbi, ifm, nivmpi, ibid,&
                               ibid, k24ir2, k24b, k24b, rbid)
    endif
end subroutine
