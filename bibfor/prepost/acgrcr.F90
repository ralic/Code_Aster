subroutine acgrcr(nbvec, jvectn, jvectu, jvectv, nbordr,&
                  kwork, sompgw, jrwork, tspaq, ipg,&
                  nommet, jvecno, jnorma, forcri, nompar,&
                  vanocr, respc, vnmax)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/loisem.h"
#include "asterc/lor8em.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/vecnuv.h"
#include "asterfort/wkvect.h"
#include "asterfort/anacri.h"
#include "asterfort/acplcr.h"
#include "asterfort/fointe.h"
#include "asterfort/fonbpa.h"
!
    integer :: nbvec, jvectn, jvectu, jvectv, nbordr, kwork
    integer :: sompgw, jrwork, tspaq, ipg, jvecno, jnorma
    character(len=16) :: nommet, forcri
    character(len=8) :: nompar(35)
    real(kind=8) :: respc(24), vnmax(6), vanocr(23)
!
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ---------------------------------------------------------------------
! BUT: POUR LA FATIGUE A AMPLITUDE CONSTANTE
!      DETERMINER LE PLAN DES MAX DES TAU_MAX ET CALCULER DES GRANDEURS
!
!
! REMARQUE: CETTE SUBROUTINE EST APPLICABLE POUR UN NOEUD OU IPG EGALE
!           A 1 ET SOMPGW = SOMNOW,JVECPG = JVECNO
! ----------------------------------------------------------------------
! ARGUMENTS :
!     JVECTN  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS NORMAUX.
!     JVECTU  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS u DU PLAN DE CISAILLEMENT.
!     JVECTV  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS v DU PLAN DE CISAILLEMENT.
!     NBORDR  : IN  : NOMBRE DE NUMEROS D'ORDRE.
!     KWORK   : IN  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET DE
!                               MAILLES ;
!                     KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
!                               MAILLES.
!     SOMPGW  : IN  : SOMME DES POINTS DE GAUSS DES N MAILLES PRECEDANT
!                     LA MAILLE COURANTE.
!     JRWORK  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
!                     L'HISTORIQUE DES TENSEURS DES CONTRAINTES
!                     ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
!                     DU <<PAQUET>> DE MAILLES.
!     TSPAQ   : IN  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
!                     COURANT.
!     IPG     : IN  : IEME POINT DE GAUSS.
!    NOMMET     IN    NOM DE METHOD D'APPROCHEMENT DE CERCLE ("CERCLE
!                     EXACT" ET "CERCLE APPROCHE")
!    VALA       IN    VALEUR DU PARAMETRE a ASSOCIE AU CRITERE.
!    COEFPA     IN    COEFFICIENT DE PASSAGE CISAILLEMENT - UNIAXIAL.
!   VRSESU      OUT   TABLEAU DES RESULTATS (GRANDEURS ET DOMMAGE).
!                     POUR L'INSTANT, LA DIMENSION DE VRESU EST 24
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
    integer :: i, j, k
    integer :: mnmax(2)
!
    integer :: nparma, praccr(35)
    integer :: ibid, jresun
    integer :: jprof, np, dectau, ipar
    integer :: jdtaum, jtauma, jsgnma, jdsgma
    integer :: jdgama, jgamma, jepnma, jdenma
    integer :: jdgpma, jgapma, jeppma, jdepma
!
    real(kind=8) :: epsilo, pi
    real(kind=8) :: valpar(35), valpu(35)
    real(kind=8) :: grcrma(2), gcmax
    character(len=16) :: typch, nomcr
    character(len=24) :: chnom, cbid
    character(len=8) :: nompf(35)
    aster_logical :: lbid, crsigm, crepst, crepse, crepsp, rayon
!
    epsilo = 10*r8prem()
    pi = r8pi()
!
    typch = 'PERIODIQUE'
    nomcr = 'FORMULE_CRITERE'
!
    call anacri(nomcr, forcri, typch, 'NON', praccr,&
                lbid, crsigm, crepst, crepse, crepsp)
!
    rayon = .false.
    if ((praccr(24) .eq. 1) .or. (praccr(25) .eq. 1) .or. (praccr(32) .eq. 1)) then
        rayon = .true.
    endif
!
    do 20 i = 24, 35
        valpar(i) = 0.0d0
 20 continue
!
! Récuperer les paramètres qui ne dépendent pas de plan
    do 21 i = 7, 23
        valpar(i) = vanocr(i)
 21 continue
!
!
    call wkvect('&&ACGRCR.RESU_N', 'V V I', nbvec, jresun)
!
    if (crsigm) then
!
        call wkvect('&&ACGRCR.DTAU_MAX', 'V V R', nbvec, jdtaum)
        call wkvect('&&ACGRCR.TAUMAX', 'V V R', nbvec, jtauma)
        call wkvect('&&ACGRCR.SGNMAX', 'V V R', nbvec, jsgnma)
        call wkvect('&&ACGRCR.DSNMAX', 'V V R', nbvec, jdsgma)
!
        dectau = 0
        call acplcr(nbvec, jvectn, jvectu, jvectv, nbordr,&
                    kwork, sompgw, jrwork, tspaq, ipg,&
                    dectau, nommet, jvecno, jnorma, rayon,&
                    jresun, jdtaum, jtauma, jsgnma, jdsgma)
!
    endif
!
    if (crepst) then
        call wkvect('&&ACGRCR.DGAMAX', 'V V R', nbvec, jdgama)
        call wkvect('&&ACGRCR.GAMMAX', 'V V R', nbvec, jgamma)
        call wkvect('&&ACGRCR.EPNMAX', 'V V R', nbvec, jepnma)
        call wkvect('&&ACGRCR.DENMAX', 'V V R', nbvec, jdenma)
!
        dectau = 6
        call acplcr(nbvec, jvectn, jvectu, jvectv, nbordr,&
                    kwork, sompgw, jrwork, tspaq, ipg,&
                    dectau, nommet, jvecno, jnorma, rayon,&
                    jresun, jdgama, jgamma, jepnma, jdenma)
!
    endif
!
    if (crepsp) then
        call wkvect('&&ACGRCR.DGPMAX', 'V V R', nbvec, jdgpma)
        call wkvect('&&ACGRCR.GAPMAX', 'V V R', nbvec, jgapma)
        call wkvect('&&ACGRCR.EPPMAX', 'V V R', nbvec, jeppma)
        call wkvect('&&ACGRCR.DEPMAX', 'V V R', nbvec, jdepma)
!
        dectau = 12
        call acplcr(nbvec, jvectn, jvectu, jvectv, nbordr,&
                    kwork, sompgw, jrwork, tspaq, ipg,&
                    dectau, nommet, jvecno, jnorma, rayon,&
                    jresun, jdgpma, jgapma, jeppma, jdepma)
!
    endif
!
!
! 3/ CDU 1ER MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE
!
    grcrma(1) = r8prem()
    grcrma(2) = r8prem()
    mnmax(1) = 1
    mnmax(2) = 1
!
    gcmax = 0.0d0
!
!  RECUPERER LES NOMS DE PARAMETRES FOURNIS PAR L'UTILISATEUR
!  NOMBRE DE PARAMETRES DISPONIBLES
!
    nparma = 35
!
!
    chnom(20:24) = '.PROL'
    chnom(1:19) = forcri
!
    call jeveuo(chnom, 'L', jprof)
    call fonbpa(forcri, zk24(jprof), cbid, nparma, np,&
                nompf)
!
!
    do 430 i = 1, nbvec
        if (crsigm) then
            valpar(24) = zr(jdtaum + i-1)
            valpar(26) = zr(jdsgma + i-1)
            valpar(28) = zr(jtauma + i-1)
            valpar(30) = zr(jsgnma + i-1)
        endif
!
        if (crepst) then
!! POUR ENGEERING STRAIN
            valpar(25) = zr(jdgama + i-1)*2
            valpar(27) = zr(jdenma + i-1)
            valpar(29) = zr(jgamma + i-1)*2
            valpar(31) = zr(jepnma + i-1)
        endif
        if (crepsp) then
            valpar(32) = zr(jdgpma + i-1)*2
            valpar(33) = zr(jdepma + i-1)
            valpar(34) = zr(jgapma + i-1)*2
            valpar(35) = zr(jeppma + i-1)
        endif
!
        do 30 j = 1, np
            do 25 ipar = 1, nparma
                if (nompf(j) .eq. nompar(ipar)) then
                    valpu(j) = valpar(ipar)
                    goto 30
                endif
 25         continue
 30     continue
        !
        call fointe('F', forcri, np, nompf, valpu,&
                    gcmax, ibid)
!
        if (gcmax .gt. epsilo) then
            if ((gcmax-grcrma(1)) .gt. epsilo) then
                grcrma(2) = grcrma(1)
                mnmax(2) = mnmax(1)
                grcrma(1) = gcmax
                mnmax(1) = i
!
            endif
            if (( abs(grcrma(2)-grcrma(1)) .lt. epsilo ) .and. (i .ne. mnmax(1))) then
                grcrma(2) = gcmax
                mnmax(2) = i
            endif
        endif
430 end do
!!!RECUPER DES VALUERS DE LA GRANDEUR CRITIQUE
    do 440 k = 1, 2
        i = mnmax(k)
!
        if (crsigm) then
            valpar(24) = zr(jdtaum + i-1)
            valpar(26) = zr(jdsgma + i-1)
            valpar(28) = zr(jtauma + i-1)
            valpar(30) = zr(jsgnma + i-1)
        endif
!
!! *2 POUR ENGEERING STRAIN
        if (crepst) then
            valpar(25) = zr(jdgama + i-1)*2
            valpar(27) = zr(jdenma + i-1)
            valpar(29) = zr(jgamma + i-1)*2
            valpar(31) = zr(jepnma + i-1)
        endif
!
        if (crepsp) then
            valpar(32) = zr(jdgpma + i-1)*2
            valpar(33) = zr(jdepma + i-1)
            valpar(34) = zr(jgapma + i-1)*2
            valpar(35) = zr(jeppma + i-1)
        endif
!
        do 450 j = 1, 12
            respc(j+(k-1)*12) = valpar(23+j)
450     continue
!
!!!ORIENTATION DU PLAN CRITIQUE
        vnmax(3*(k-1)+1) = zr(jvectn + (i-1)*3)
        vnmax(3*(k-1)+2) = zr(jvectn + (i-1)*3 + 1)
        vnmax(3*(k-1)+3) = zr(jvectn + (i-1)*3 + 2)
!
440 continue
!
    if (crsigm) then
        call jedetr('&&ACGRCR.TAUMAX')
        call jedetr('&&ACGRCR.SGNMAX')
        call jedetr('&&ACGRCR.DSNMAX')
    endif
!
    if (crepst) then
        call jedetr('&&ACGRCR.DGAMAX')
        call jedetr('&&ACGRCR.GAMMAX')
        call jedetr('&&ACGRCR.EPNMAX')
        call jedetr('&&ACGRCR.DENMAX')
    endif
!
    if (crepsp) then
        call jedetr('&&ACGRCR.DGPMAX')
        call jedetr('&&ACGRCR.GAPMAX')
        call jedetr('&&ACGRCR.EPPMAX')
        call jedetr('&&ACGRCR.DEPMAX')
    endif
!
    call jedetr('&&ACGRCR.DTAU_MAX')
    call jedetr('&&ACGRCR.RESU_N')
!
end subroutine
