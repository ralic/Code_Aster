subroutine fetinl(nbi, vlagi, matas, chsecm, lrigid,&
                  dimgi, nbsd, vsdf, vddl, nomggt,&
                  ipiv, nomgi, lstogi, infofe, irex,&
                  ifm, sdfeti, nbproc, rang, k24lai,&
                  itps)
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
!    - FONCTION REALISEE:  CALCUL DU VECTEUR LAGRANGE INITIAL LANDA0
!
!   IN   NBI   : IN  : TAILLE DU VECTEUR
!   IN/OUT VLAGI : VR8: VECTEUR LAGRANGE INITIAL
!   IN   MATAS: K19  : NOM DE LA MATRICE DE RIGIDITE GLOBALE
!   IN  CHSECM: K19  : CHAM_NO SECOND MEMBRE GLOBAL
!   IN  LRIGID: LO   : LOGICAL INDIQUANT LA PRESENCE D'AU MOINS UN
!         SOUS-DOMAINES FLOTTANT
!   IN  DIMGI:  IN   : TAILLE DE GIT*GI
!   IN   NBSD:  IN   : NOMBRE DE SOUS-DOMAINES
!   IN   VSDF: VIN  : VECTEUR MATR_ASSE.FETF INDIQUANT SI SD FLOTTANT
!   IN   VDDL: VIN  : VECTEUR DES NBRES DE DDLS DES SOUS-DOMAINES
!   IN  SDFETI: CH19: SD DECRIVANT LE PARTIONNEMENT FETI
!   IN/OUT IPIV: VIN : ADRRESSE VECTEUR DECRIVANT LE PIVOTAGE LAPACK
!                     POUR INVERSER (GIT)*GI
!   IN LSTOGI: LO : TRUE, GI STOCKE, FALSE, RECALCULE
!   IN IREX  : IN    : ADRESSE DU VECTEUR AUXILAIRE EVITANT DES APPELS
!                        JEVEUX.
!   IN RANG  : IN  : RANG DU PROCESSEUR
!   IN NBPROC: IN  : NOMBRE DE PROCESSEURS
!   IN K24LAI: K24 : NOM DE L'OBJET JEVEUX VLAGI POUR LE PARALLELISME
!   IN NOMGI/NOMGGT: K24 : NOM DES OBJETS JEVEUX GI ET GIT*GI
!   IN ITPS  : IN  : NUMERO DU PAS DE TEMPS
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
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "blas/daxpy.h"
#include "blas/ddot.h"
#include "blas/dgemv.h"
#include "blas/dnrm2.h"
#include "blas/dsptrf.h"
#include "blas/dsptrs.h"
    integer :: nbi, dimgi, nbsd, vsdf(nbsd), vddl(nbsd), ipiv, itps, irex, ifm
    integer :: nbproc, rang
    real(kind=8) :: vlagi(nbi)
    character(len=19) :: matas, chsecm, sdfeti
    character(len=24) :: infofe, k24lai, nomgi, nomggt
    logical :: lrigid, lstogi
!
!
! DECLARATION VARIABLES LOCALES
    integer :: i, jve, idd, ifetr, nbmc, imc, nbmc1, idecai, ibid, ivale, idecao
    integer :: nbddl, ifetc, jve1, gii, infol8, ilimpi, dimgi1, nivmpi, jgi
    integer :: jgitgi
    integer(kind=4) :: infola, dimgi4, nbi4
    real(kind=8) :: raux, rbid
    character(len=8) :: nomsd
    character(len=19) :: chsmdd
    character(len=24) :: nomsdr, sdfetg, k24er, k24bid
    logical :: lpara
!
! CORPS DU PROGRAMME
    call jemarq()
!
    call matfpe(-1)
!
! INITS.
    dimgi4=dimgi
    nbi4=nbi
    if (infofe(10:10) .eq. 'T') then
        nivmpi=2
    else
        nivmpi=1
    endif
    if (nbproc .eq. 1) then
        lpara=.false.
    else
        lpara=.true.
    endif
    sdfetg=sdfeti//'.FETG'
    nomsdr=matas//'.FETR'
    do 10 i = 1, nbi
        vlagi(i)=0.d0
10  end do
!
! SI MODES DE CORPS RIGIDES CALCUL DE LANDA0=GI*(GITGI)-1*E
    if (lrigid) then
!
! EN PARALLELE, GI ET GIT*GI NE SONT STOCKES QUE PAR LE PROC 0
        if (rang .eq. 0) then
            if (lstogi) call jeveuo(nomgi, 'L', jgi)
            call jeveuo(nomggt, 'E', jgitgi)
        endif
!
! ADRESSE JEVEUX OBJET FETI & MPI
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
! OBJET JEVEUX POINTANT SUR LA LISTE DES CHAM_NO SECOND MEMBRE
        call jeveuo(chsecm//'.FETC', 'L', ifetc)
!
! VECTEUR AUXILLIAIRE CONTENANT VECTEUR E=[F1T*B1...FQT*BQ]T
! EN PARALLELE, POUR RANG DIFFERENT DE ZERO, IL SERT JUSTE AU
! MPI_REDUCE
        k24er='&&FETINL.E.R'
        call wkvect(k24er, 'V V R', dimgi, jve)
        dimgi1=dimgi-1
        do 20 i = 0, dimgi1
            zr(jve+i)=0.d0
20      continue
!
! --------------------------------------------------------------------
! CONSTITUTION DE E STOCKE DANS '&&FETINL.E.R'
! --------------------------------------------------------------------
!
! DECALAGE STOCKAGE E
        idecao=jve
!
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
        do 100 idd = 1, nbsd
!
! NOMBRES DE MODES DE CORPS RIGIDES DU SOUS-DOMAINE IDD
            nbmc=vsdf(idd)
! LE SOUS-DOMAINE IDD EST IL CONCERNE PAR LE PROCESSUS ACTUEL ?
            if (zi(ilimpi+idd) .eq. 1) then
                call jemarq()
                if (nbmc .ne. 0) then
! SOUS-DOMAINE FLOTTANT
                    nbmc1=nbmc-1
!
! NBRE DE DDL DU SOUS-DOMAINE IDD
                    nbddl=vddl(idd)
!
! COMPOSANTES DES MODES DE CORPS RIGIDES
                    call jenuno(jexnum(sdfetg, idd), nomsd)
                    call jeveuo(jexnom(nomsdr, nomsd), 'L', ifetr)
! SECOND MEMBRE LOCAL AU SOUS-DOMAINE
                    chsmdd=zk24(ifetc+idd-1)(1:19)
                    call jeveuo(chsmdd//'.VALE', 'L', ivale)
!
! ----  BOUCLE SUR LES MODES DE CORPS RIGIDES
! DECALAGE DE .FETR
                    idecai=ifetr
                    do 90 imc = 0, nbmc1
                        zr(idecao)=ddot(nbddl,zr(ivale),1,zr(idecai),&
                        1)
                        idecai=idecai+nbddl
                        idecao=idecao+1
90                  continue
                endif
                call jedema()
            else
! EN PARALLELE, IL FAUT DECALER L'OUTPUT POUR PRENDRE EN COMPTE LES
! MODES DE CORPS RIGIDES DES AUTRES PROCS
                if (lpara) idecao=idecao+nbmc
            endif
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
100      continue
! REDUCTION DE E POUR LE PROCESSUS MAITRE
        if (lpara) call fetmpi(7, dimgi, ifm, nivmpi, ibid,&
                               ibid, k24er, k24bid, k24bid, rbid)
! MONITORING
        if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETINL', rang, '> CONSTRUCTION DE E'
        if ((infofe(4:4).eq.'T') .and. (rang.eq.0)) then
            if (dimgi .le. 5) then
                do 105 i = 1, dimgi
                    write(ifm,*)'E(I)',i,zr(jve+i-1)
105              continue
            endif
            raux=dnrm2(dimgi4,zr(jve),1)
            write(ifm,*)'NORME DE E ',raux
        endif
! --------------------------------------------------------------------
! CONSTITUTION DE ((GI)T*GI)-1*E STOCKE DANS '&&FETINL.E.R'
! --------------------------------------------------------------------
! EN PARALLELE SEUL LE PROCESSUS MAITRE CONSTRUIT CET OBJET
        if (rang .eq. 0) then
            if (itps .eq. 1) then
                infola=0
                infol8=0
! FACTORISATION/DESCENTE-REMONTEE SYMETRIQUE INDEFINIE (STOCKEE PAR
! PAQUET) VIA LAPACK. A NE FAIRE Qu'AU PREMIER PAS DE TEMPS
                call dsptrf('L', dimgi4, zr(jgitgi), zi4(ipiv), infola)
                infol8=infola
                call assert(infol8.eq.0)
            endif
            infol8=0
            infola=0
            call dsptrs('L', dimgi4, 1, zr(jgitgi), zi4(ipiv),&
                        zr(jve), dimgi4, infola)
            infol8=infola
            call assert(infol8.eq.0)
! MONITORING
            if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETINL', rang,&
                                      '> INVERSION (GITGI)-1*E'
            if (infofe(4:4) .eq. 'T') then
                if (dimgi .le. 5) then
                    do 115 i = 1, dimgi
                        write(ifm,*)'(GIT*GI)-1*E(I)',i,zr(jve+i-1)
115                  continue
                endif
                raux=dnrm2(dimgi4,zr(jve),1)
                write(ifm,*)'NORME DE (GIT*GI)-1*E ',raux
            endif
!
! --------------------------------------------------------------------
! CONSTITUTION DE LANDA0=GI*(((GI)T*GI)-1*E) STOCKE DANS VLAGI
! --------------------------------------------------------------------
!
            if (lstogi) then
                call dgemv('N', nbi4, dimgi4, 1.d0, zr(jgi),&
                           nbi4, zr(jve), 1, 1.d0, vlagi,&
                           1)
            else
! SANS CONSTRUIRE GI, SEULEMENT EN SEQUENTIEL
                call wkvect('&&FETI.GGT.V3', 'V V R', nbi, gii)
                jve1=jve-1
                do 200 idd = 1, nbsd
! NBRE DE DDL DU SOUS-DOMAINE IDD
                    nbddl=vddl(idd)
! NOMBRES DE MODES DE CORPS RIGIDES DU SOUS-DOMAINE IDD
                    nbmc=vsdf(idd)
!
                    if (nbmc .ne. 0) then
! SOUS-DOMAINE FLOTTANT
! COMPOSANTES DES MODES DE CORPS RIGIDES DE IDD
                        call jenuno(jexnum(sdfetg, idd), nomsd)
                        call jeveuo(jexnom(nomsdr, nomsd), 'L', ifetr)
! ----  BOUCLES SUR LES MODES DE CORPS RIGIDES DE IDD
                        do 190 imc = 1, nbmc
                            jve1=jve1+1
                            raux=zr(jve1)
                            call fetrex(1, idd, nbddl, zr(ifetr+(imc-1)* nbddl), nbi,&
                                        zr(gii), irex)
                            call daxpy(nbi4, raux, zr(gii), 1, vlagi,&
                                       1)
190                      continue
                        call jelibe(jexnom(nomsdr, nomsd))
                    endif
200              continue
                call jedetr('&&FETI.GGT.V3')
!
            endif
!
! MONITORING
            if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETINL', rang,&
                                      '> CONSTRUCTION DE LANDA0'
            if (infofe(4:4) .eq. 'T') then
                if (dimgi .le. 5) then
                    do 205 i = 1, nbi
                        write(ifm,*)'LANDA0(I)',i,vlagi(i)
205                  continue
                endif
                raux=dnrm2(nbi4,vlagi,1)
                write(ifm,*)'NORME DE LANDA0 ',raux
            endif
!
! FIN DU IF RANG
        endif
! DESTRUCTION OBJET TEMPORAIRE
        call jedetr(k24er)
! FIN DU IF LRIGID
    endif
! EN PARALLELE, ENVOI DE VLAGI A TOUS LES PROC POUR CALCULER LE RESIDU
    if (lpara) call fetmpi(9, nbi, ifm, nivmpi, ibid,&
                           ibid, k24lai, k24bid, k24bid, rbid)
!
    call matfpe(1)
!
    call jedema()
end subroutine
