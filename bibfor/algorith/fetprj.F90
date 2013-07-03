subroutine fetprj(nbi, vi, vo, nomggt, lrigid,&
                  dimgi, option, sdfeti, ipiv, nbsd,&
                  vsdf, vddl, matas, nomgi, lstogi,&
                  infofe, irex, iprj, nbproc, rang,&
                  k24irg)
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
!    - FONCTION REALISEE:  CALCUL AU SENS FETI DE:
!          * LA PROJECTION COMPLETE P=I-GI.(GIT.GI)-1.GIT SI OPTION=1
!               P(VI) EST STOCKEE DANS LES NBI COMPOSANTES DE VO
!          * LA PROJECTION PARTIELLE P'=(GIT.GI)-1.GIT SI OPTION=2
!               P'(VI) EST STOCKEE DANS LES DIMGI PREMIERES COMPOSANTES
!                DE VO
!
!      IN    NBI: IN   : NOMBRE DE NOEUDS D'INTERFACE
!      IN     VI: VR8  : VECTEUR INPUT DE TAILLE NBI
!      OUT    VO: VR8  : VECTEUR OUTPUT DE TAILLE NBI SI OPTION=1
!      IN JGITGI: IN  : ADRESSE OBJET JEVEUX (GI)T*GI
!      IN LRIGID: LO  : LOGICAL INDIQUANT LA PRESENCE D'AU MOINS UN
!         SOUS-DOMAINES FLOTTANT
!      IN  DIMGI:  IN : TAILLE DE GIT*GI
!      IN OPTION:  IN  : 1 -> PROJECTION., 2-> RECONSTRUCTION ALPHA SOL.
!      IN SDFETI: CH19 : SD DECRIVANT LE PARTIONNEMENT FETI
!   IN/OUT IPIV: VIN : ADRESSE VECTEUR DECRIVANT LE PIVOTAGE LAPACK
!                     POUR INVERSER (GIT)*GI
!     IN   VSDF: VIN : VECTEUR MATR_ASSE.FETF INDIQUANT SI SD FLOTTANT
!     IN   VDDL: VIN : VECTEUR DES NBRES DE DDLS DES SOUS-DOMAINES
!     IN   NBSD:  IN : NOMBRE DE SOUS-DOMAINES
!     IN   MATAS: K19 : NOM DE LA MATRICE DE RIGIDITE GLOBALE
!     IN    GI : MATR8: MATRICE GI
!     IN  LSTOGI: LO : TRUE, GI STOCKE, FALSE, RECALCULE
!     IN IREX/IPRJ: IN : ADRESSE DU VECTEUR AUXILAIRE EVITANT DES APPELS
!                        JEVEUX.
!     IN RANG  : IN  : RANG DU PROCESSEUR
!     IN NBPROC: IN  : NOMBRE DE PROCESSEURS
!     IN K24IRG : K24 : NOM DE L'OBJET JEVEUX VDO POUR LE PARALLELISME
!                   SI K24IRG='VIDE', ON NE FAIT PAS LE MPI_BCAST FINAL
!    IN NOMGI/NOMGGT: K24 : NOM DES OBJETS JEVEUX GI ET GIT*GI
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
! aslint: disable=W1304,W1504
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
!
#include "asterc/matfpe.h"
#include "asterfort/assert.h"
#include "asterfort/fetmpi.h"
#include "asterfort/fetrex.h"
#include "asterfort/jelibe.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mess.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dgemv.h"
#include "blas/dsptrs.h"
    integer :: nbsd, nbi, dimgi, option, vsdf(nbsd), vddl(nbsd), ipiv, irex
    integer :: iprj, rang, nbproc
    real(kind=8) :: vi(nbi), vo(nbi), rbid
    logical :: lrigid, lstogi
    character(len=19) :: sdfeti, matas
    character(len=24) :: infofe, k24irg, nomggt, nomgi
!
!
! DECLARATION VARIABLES LOCALES
    integer :: jgitvi, jgitv1, i, ifm, nivmpi, gii, gii1, nbddl, nbmc, ifetr
    integer :: imc, idd, infol8, ibid, ibcast, jgi, jgitgi
    integer(kind=4) :: infola
    real(kind=8) :: raux
    character(len=8) :: nomsd
    character(len=24) :: nomsdr, sdfetg, k24b
    logical :: lpara
    integer(kind=4) :: dimgi4, nbi4
!
!
    call matfpe(-1)
!
! INITS DIVERSES
    dimgi4=dimgi
    nbi4=nbi
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
! ROUTINE AVEC MOINS DE MONITORING, JEVEUX.. CAR APPELLEE SOUVENT
    ifm=zi(iprj)
!
! EN PARALLELE SEUL LE PROCESSUS MAITRE CONSTRUIT CET OBJET VD0
    if (rang .eq. 0) then
!
        call assert((option.eq.1).or.(option.eq.2))
        sdfetg=sdfeti//'.FETG'
!---------------------------------------------------------------------
! --------------------------------------------------------------------
! AUCUN MODE DE CORPS RIGIDE P=ID (OPTION=1 OBLIGEATOIRE)
! --------------------------------------------------------------------
!---------------------------------------------------------------------
!
        if (.not.lrigid) then
!
            if (option .ne. 1) call u2mess('F', 'ALGORITH3_66')
            call dcopy(nbi4, vi, 1, vo, 1)
!
        else
!---------------------------------------------------------------------
! --------------------------------------------------------------------
! PRESENCE DE MODES DE CORPS RIGIDES P (OPTION=1) OU P' (OPTION=2)
! --------------------------------------------------------------------
!---------------------------------------------------------------------
!
! EN PARALLELE, GI ET GIT*GI NE SONT STOCKES QUE PAR LE PROC 0
            if (lstogi) call jeveuo(nomgi, 'L', jgi)
            call jeveuo(nomggt, 'L', jgitgi)
!
! --------------------------------------------------------------------
! CONSTITUTION DE (GI)T*VI STOCKE DANS '&&FETPRJ.GITVI.R'
! --------------------------------------------------------------------
            jgitvi=zi(iprj+1)
            jgitv1=jgitvi-1
!
            if (lstogi) then
                call dgemv('T', nbi4, dimgi4, 1.d0, zr(jgi),&
                           nbi4, vi, 1, 0.d0, zr(jgitvi),&
                           1)
            else
! SANS CONSTRUIRE GI, SEULEMENT EN SEQUENTIEL
                do 9 i = 1, dimgi
                    zr(jgitv1+i)=0.d0
 9              continue
                nomsdr=matas//'.FETR'
                gii=zi(iprj+2)
                gii1=gii-1
                do 30 idd = 1, nbsd
                    nbddl=vddl(idd)
                    nbmc=vsdf(idd)
                    if (nbmc .ne. 0) then
                        call jenuno(jexnum(sdfetg, idd), nomsd)
                        call jeveuo(jexnom(nomsdr, nomsd), 'L', ifetr)
                        do 20 imc = 1, nbmc
                            jgitv1=jgitv1+1
                            call fetrex(1, idd, nbddl, zr(ifetr+(imc-1)* nbddl), nbi,&
                                        zr(gii), irex)
                            do 10 i = 1, nbi
                                zr(jgitv1)=zr(jgitv1)+zr(gii1+i)*vi(i)
10                          continue
20                      continue
                        call jelibe(jexnom(nomsdr, nomsd))
                    endif
30              continue
                jgitv1=jgitvi-1
!
            endif
!
! --------------------------------------------------------------------
! CONSTITUTION DE ((GI)T*GI)-1*(GI)T*VI STOCKE DANS '&&FETPRJ.GITGI.R'
! --------------------------------------------------------------------
            infola=0
            infol8=0
! DESCENTE-REMONTEE MATRICE SYMETRIQUE INDEFINIE (STOCKEE PAR PAQUET)
! VIA LAPACK
            call dsptrs('L', dimgi4, 1, zr(jgitgi), zi4(ipiv),&
                        zr(jgitvi), dimgi4, infola)
            infol8=infola
            call assert(infol8.eq.0)
            if (option .eq. 1) then
! --------------------------------------------------------------------
! CONSTITUTION DE V0=VI-GI*((GI)T*GI)-1*(GI)T*VI (OPTION=1)
! --------------------------------------------------------------------
                call dcopy(nbi4, vi, 1, vo, 1)
!
                if (lstogi) then
                    call dgemv('N', nbi4, dimgi4, -1.d0, zr(jgi),&
                               nbi4, zr(jgitvi), 1, 1.d0, vo,&
                               1)
                else
! SANS CONSTRUIRE GI, SEULEMENT EN SEQUENTIEL
                    do 200 idd = 1, nbsd
                        nbddl=vddl(idd)
                        nbmc=vsdf(idd)
!
                        if (nbmc .ne. 0) then
                            call jenuno(jexnum(sdfetg, idd), nomsd)
                            call jeveuo(jexnom(nomsdr, nomsd), 'L', ifetr)
                            do 190 imc = 1, nbmc
                                jgitv1=jgitv1+1
                                raux=-zr(jgitv1)
                                call fetrex(1, idd, nbddl, zr(ifetr+(imc- 1)*nbddl), nbi,&
                                            zr(gii), irex)
                                call daxpy(nbi4, raux, zr(gii), 1, vo,&
                                           1)
190                          continue
                            call jelibe(jexnom(nomsdr, nomsd))
                        endif
200                  continue
!
                endif
!
! MONITORING
                if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETPRJ', rang,&
                                          '> LAMBDA = P * LAMBDA'
!
            else
! --------------------------------------------------------------------
! CONSTITUTION DE V0=((GI)T*GI)-1*(GI)T*VI (OPTION=2)
! --------------------------------------------------------------------
                call dcopy(dimgi4, zr(jgitvi), 1, vo, 1)
!
! MONITORING
                if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETPRJ', rang,&
                                          '> ALPHA = P'' * RESIDU'
            endif
! FIN DU SI LRIGID
        endif
! FIN DU SI RANG
    endif
!
! EN PARALLELE, ENVOI DE VO A TOUS LES PROC POUR PREPARER LE CALCUL
! SUIVANT
    if ((lpara) .and. (k24irg(1:4).ne.'VIDE')) then
        if (option .eq. 1) then
            ibcast=nbi
        else
            ibcast=dimgi
        endif
        call fetmpi(9, ibcast, ifm, nivmpi, ibid,&
                    ibid, k24irg, k24b, k24b, rbid)
    endif
!
    call matfpe(1)
!
end subroutine
