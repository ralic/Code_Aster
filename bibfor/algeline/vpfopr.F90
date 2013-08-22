subroutine vpfopr(option, typres, lmasse, lraide, ldynam,&
                  omemin, omemax, omeshi, nbfreq, npivot,&
                  omecor, precsh, nbrssa, nblagr, solveu,&
                  det, idet)
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     DETERMINATION DE SHIFT(S), D'UNE MATRICE SHIFTEE, DE SA FACTORISEE
!     DU NBRE DE PIVOTS NEGATIFS (POUR TEST DE STURM) VOIRE DU NBRE
!     DE FREQ DANS UNE BANDE.
!
!     POUR ETAPE DE PRETRAITEMENTS DE MODE_ITER_SIMULT
!     OPTION='CENTRE' --> OUTPUT: MATRICE SHIFTEE + SA FACTORISEE +
!                         NPIVOT(1) + OMESHI
!                         INPUT: INTENDANCE + OMEMIN
!
!     OPTION='BANDE'  --> OUTPUT: MATRICE SHIFTEE + SA FACTORISEE +
!                         NPIVOT(1) + NBFREQ + OMEMIN/MAX + OMESHI +
!                         AFFICHAGES VPECST
!                         INPUT: INTENDANCE + OMEMIN/MAX
!
!     OPTION='BANDEA'  --> OUTPUT: MATRICE SHIFTEE + SA FACTORISEE +
!                         NPIVOT(1) + OMEMIN/MAX + OMESHI +
!                         AFFICHAGES VPECST ENRICHIS + NBFREQ (JUSTE
!                         POUR AFFICHAGE)
!                         INPUT: INTENDANCE + OMEMIN/MAX + NBFREQ
!
!     OPTION='PLUS_PETITE'/'TOUT' --> OUTPUT: MATRICE SHIFTEE + SA FACTO
!                         RISEE + NPIVOT(1) + OMESHI
!                         INPUT: INTENDANCE
!
!     POUR ETAPE DE POST_TRAITEMENTS DE MODE_ITER_SIMULT
!     OPTION='STURM'  --> OUTPUT: NBFREQ + OMEMIN/MAX + PAS VPECST
!                         AFFICHAGES DEDIES EN AMONT
!                         INPUT: INTENDANCE + OMEMIN/MAX
!
!     POUR INFO_MODE + STURM AVEC LISTE DE FREQ OU CHAR_CRIT
!     PREMIERE BANDE
!     OPTION='STURML1' --> OUTPUT: NBFREQ + OMEMIN/MAX +
!                         AFFICHAGES DEDIE VPECST + NPIVOT(2)
!                         INPUT: INTENDANCE + OMEMIN/MAX
!         ='STURML1P' ...     IDEM + COMM POUR MACRO // (ETAPE INITIALE)
!         ='STURML10/11'...IDEM + COMM POUR MACRO // (ETAPE FINALE)
!
!     BANDES SUIVANTES
!     OPTION='STURMLN' --> OUTPUT: NBFREQ + OMEMIN/MAX +
!                         AFFICHAGES DEDIE VPECST + NPIVOT(2)
!                         INPUT: INTENDANCE + OMEMIN/MAX +
!                         NPIVOT(1)=NPIVOT OMEMIN ET NPIVOT(2)=
!                            NUMERO DE LA BANDE CONSIDEREE
!                         OUTPUT:NPIVOT(2)=NPIVOT OMEMAX.
!           ='STURMLNP' ... IDEM + COMM POUR MACRO //
!
!     POUR ETAPE PRETRAITEMENT DE MODE_ITER_INV (AJUSTE/SEPARE)
!     OPTION='STURMAD' --> OUTPUT: NBFREQ + OMEMIN/MAX +
!                     AFFICHAGES VPECST + DET(2)/IDET(2) + NPIVOT(2)
!                          INPUT: INTENDANCE + OMEMIN/MAX
!     ------------------------------------------------------------------
! IN  OPTION  : TX : CHOIX DE L'OPTION (PLUS_PETITE,CENTRE,BANDE,STURM)
! IN  TYPRES  : TX : TYPE DU CALCUL (DYNAMIQUE OU FLAMBEMENT)
! IN  LMASSE  : IS : DESCRIPTEUR DE LA MATRICE SECOND MEMBRE
! IN  LRAIDE  : IS : DESCRIPTEUR DE LA MATRICE PREMIER MEMBRE
! IN/OUT LDYNAM :IS : POINTEUR SUR LA FACTORISEE DE LA MATRICE DYNAMIQUE
!                    INDUITE PAR L'OPTION
! IN/OUT OMEMIN : R8 : VALEUR INFERIEURE DE LA BANDE DE RECHERCHE
!                      OU VALEUR DE DEPART POUR LES AUTRES OPTIONS
! IN/OUT OMEMAX : R8 : VALEUR SUPERIEURE DE LA BANDE DE RECHERCHE
!    OUT OMESHI : R8 : VALEUR DU SHIFT  DE LA MATRICE DE TRAVAIL
!    OUT NBFREQ : IS : NOMBRE DE FREQUENCES DANS LA BANDE
! IN/OUT NPIVOT : IS : VECTEUR NOMBRE DE PIVOTS NEGATIFS DE LA MATRICE
!                      DE TRAVAIL FACTORISEE.
!                      ATTENTION PARAMETRE PARFOIS UTILISE
!                      EN INPUT AVEC UN SENS DIFFERENT CF. OPTION.
! IN  OMECOR : R8 : VALEUR DE LA PULSATION AU CARRE DEFINISSANT LES
!                   MODES DE CORPS RIGIDE
! IN  PRECSH : R8 : VALEUR DU DECALAGE DES SHIFTS QUAND LA MATRICE EST
!                   NON INVERSIBLE (CALC_FREQ/PREC_SHIFT)
! IN  NBRSSA : IS : NOMBRE DE DECALAGES DE SHIFTS AUTORISES
! IN  NBLAGR : IS : NOMBRE DE DDLS DE LAGRANGE
! IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
! OUT  DET   : R8  : VECTEUR DES DEUX MANTISSES DE DETERMINANT
! OUT  IDET  : IS  : IDEM SUR LES EXPOSANTS
!----------------------------------------------------------------------
!
    implicit none
!
! PARAMETRES D'APPEL
#include "aster_types.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/freqom.h"
#include "asterfort/infniv.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mpicm1.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mesr.h"
#include "asterfort/u2mess.h"
#include "asterfort/vecint.h"
#include "asterfort/vpecst.h"
#include "asterfort/vpstur.h"
#include "asterfort/wkvect.h"
    character(len=*) :: option
    character(len=16) :: typres
    character(len=19) :: solveu
    integer :: lmasse, lraide, ldynam, nbrssa
    real(kind=8) :: omemin, omemax, omeshi, omecor, precsh, det(2)
    integer :: nbfreq, npivot(2), nblagr, idet(2)
!
!
! VARIABLES LOCALES
    mpi_int :: mpicou, mpicow, rang, rangl, nbproc
    character(len=1) :: typep, k1bid
    character(len=8) :: k8bid
    character(len=16) :: ch16, valk(3)
    character(len=24) :: k24c, k24par
    integer :: niv, ifm, nbessa, ier, nbfmin, nbfmax, ibid, ibande
    integer :: jk24c, jkpar, nbrow, frecou
    real(kind=8) :: valr(2), omgmin, omgmax, omgshi, rbid, prec, omgdec
    complex(kind=8) :: cbid
    logical :: caldet, ldyna
!
    call infniv(ifm, niv)
! MAUVAISE VALEUR DE OPTION
    if ((option.ne.'CENTRE') .and. (option.ne.'BANDE') .and. (option.ne.'BANDEA') .and.&
        (option.ne.'PLUS_PETITE') .and. (option.ne.'TOUT') .and. (option.ne.'STURM') .and.&
        (option.ne.'STURML1') .and. (option.ne.'STURML1P') .and. (option.ne.'STURML10') .and.&
        (option.ne.'STURML11') .and. (option.ne.'STURMLN') .and. (option.ne.'STURMLNP') .and.&
        (option.ne.'STURMAD')) ASSERT(.false.)
    det(1)=-9999.d0
    det(2)=-9999.d0
    idet(1)=-9999
    idet(2)=-9999
    ibande=1
    if (option(1:7) .ne. 'STURMLN') then
        npivot(1)=-9999
        npivot(2)=-9999
    else
        ibande=npivot(2)
        npivot(2)=-9999
    endif
    if (option(1:7) .eq. 'STURML1') ibande=1
    if (option .eq. 'STURMAD') then
        caldet=.true.
    else
        caldet=.false.
    endif
    valk(1)='FREQ'
    valk(2)='SEUIL_FREQ'
    valk(3)='MATR_RIGI'
    if (typres .eq. 'DYNAMIQUE') then
        ldyna=.true.
    else
        ldyna=.false.
        valk(1)='CHAR_CRIT'
        valk(2)='SEUIL_CHAR_CRIT'
        if (typres .eq. 'GENERAL') valk(3)='MATR_A'
    endif
    if (option(1:5) .ne. 'STURM') then
        if (option(1:5) .eq. 'BANDE') then
            call u2mesk('I', 'ALGELINE6_41', 1, 'BANDE')
        else
            call u2mesk('I', 'ALGELINE6_41', 1, option)
        endif
    endif
!     ------------------------------------------------------------------
!     ------------------------ OPTION CENTRE ---------------------------
!     ------------------------------------------------------------------
!
    if (option .eq. 'CENTRE') then
!
        omgshi = omemin
        nbessa = 0
        prec=precsh
10      continue
        ier=0
        call vpstur(lraide, omgshi, lmasse, ldynam, rbid,&
                    ibid, npivot(1), ier, solveu, .false.,&
                    .true.)
        if (ier .ne. 0) then
            nbessa= nbessa+1
            if (nbessa .le. nbrssa) then
                if (abs(omgshi) .lt. omecor) then
                    omgshi=omecor
                    if (ldyna) then
                        valr(1)=freqom(omgshi)
                    else
                        valr(1)=omgshi
                    endif
                    if (niv .ge. 1) call u2mesr('I', 'ALGELINE6_44', 1, valr)
! --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                    nbessa=nbrssa
                else
                    omgdec=max(omecor,prec*abs(omgshi))
                    omgshi=omgshi+omgdec
                    if (ldyna) then
                        valr(1)=freqom(omgshi)
                    else
                        valr(1)=omgshi
                    endif
                    if (niv .ge. 1) then
                        valr(2)=prec*100.d0
                        call u2mesr('I', 'ALGELINE6_45', 2, valr)
                    endif
                    prec=2.d0*prec
                endif
                goto 10
            else
                if (ldyna) then
                    valr(1)=freqom(omgshi)
                else
                    valr(1)=omgshi
                endif
                call u2mesg('F', 'ALGELINE3_81', 3, valk, 0,&
                            0, 1, valr)
            endif
!
        endif
        omeshi=omgshi
        if (niv .ge. 1) then
            if (ldyna) then
                call u2mesr('I', 'ALGELINE6_42', 1, freqom(omeshi))
            else
                call u2mesr('I', 'ALGELINE6_43', 1, omeshi)
            endif
        endif
!
!     ------------------------------------------------------------------
!     ------------------------ OPTION BANDE* OU STURM** ----------------
!     ------------------------------------------------------------------
!
        else if ((option(1:5).eq.'BANDE').or. (option(1:5).eq.'STURM'))&
    then
!
        omgmin=omemin
        if ((option.eq.'STURMLN') .or. (option.eq.'STURMLNP')) then
            nbfmin=npivot(1)
            else if ((option.ne.'BANDEA').and.(option.ne.'STURML11'))&
        then
            nbessa=0
            prec=precsh
21          continue
            ier=0
            call vpstur(lraide, omgmin, lmasse, ldynam, det(1),&
                        idet(1), npivot(1), ier, solveu, caldet,&
                        .false.)
            nbfmin=npivot(1)
            if (ier .ne. 0) then
                nbessa=nbessa+1
                if (nbessa .le. nbrssa) then
                    if (abs(omgmin) .lt. omecor) then
                        omgmin=-omecor
                        if (ldyna) then
                            valr(1)=freqom(omgmin)
                        else
                            valr(1)=omgmin
                        endif
                        if (niv .ge. 1) call u2mesr('I', 'ALGELINE6_46', 1, valr)
! --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                        nbessa=nbrssa
                    else
                        omgdec=max(omecor,prec*abs(omgmin))
                        omgmin=omgmin-omgdec
                        if (ldyna) then
                            valr(1)=freqom(omgmin)
                        else
                            valr(1)=omgmin
                        endif
                        if (niv .ge. 1) then
                            valr(2)=prec*100.d0
                            call u2mesr('I', 'ALGELINE6_47', 2, valr)
                        endif
                        prec=2.d0*prec
                    endif
                    goto 21
                else
                    call u2mess('A+', 'ALGELINE3_82')
                    call u2mesk('A', 'ALGELINE3_84', 1, valk(2))
                endif
            endif
        endif
        omemin=omgmin
        if (omemin .ge. omemax) call u2mess('F', 'ALGELINE3_85')
!
        omgmax=omemax
        if ((option.ne.'BANDEA') .and. (option.ne.'STURML10')) then
            nbessa=0
            prec=precsh
22          continue
            ier=0
            call vpstur(lraide, omgmax, lmasse, ldynam, det(2),&
                        idet(2), npivot(2), ier, solveu, caldet,&
                        .false.)
            nbfmax=npivot(2)
            if (ier .ne. 0) then
                nbessa=nbessa+1
                if (nbessa .le. nbrssa) then
                    if (abs(omgmax) .lt. omecor) then
                        omgmax=omecor
                        if (ldyna) then
                            valr(1)=freqom(omgmax)
                        else
                            valr(1)=omgmax
                        endif
                        if (niv .ge. 1) call u2mesr('I', 'ALGELINE6_48', 1, valr)
! --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                        nbessa=nbrssa
                    else
                        omgdec=max(omecor,prec*abs(omgmax))
                        omgmax=omgmax+omgdec
                        if (ldyna) then
                            valr(1)=freqom(omgmax)
                        else
                            valr(1)=omgmax
                        endif
                        if (niv .ge. 1) then
                            valr(2)=prec*100.d0
                            call u2mesr('I', 'ALGELINE6_49', 2, valr)
                        endif
                        prec=2.d0*prec
                    endif
                    goto 22
                else
                    call u2mess('A+', 'ALGELINE3_83')
                    call u2mesk('A', 'ALGELINE3_84', 1, valk(2))
                endif
            endif
        endif
        omemax=omgmax
!
!     ------------------------------------------------------------------
!     ---------INFO_MODE OU MACRO_MODE_MECA PARALLELE (PART I) ---------
!     ------------------------------------------------------------------
!     --- COMMUNICATION DES PIVOTS POUR LE BON CALCUL DE STURM
        if ((option.eq.'STURML1P') .or. (option.eq.'STURMLNP') .or. (option.eq.'STURML10')&
            .or. (option.eq.'STURML11')) then
            call asmpi_comm('GET_WORLD', mpicow)
            call asmpi_comm('GET', mpicou)
            ASSERT(mpicou .ne. mpicow)
!         --- ON REMPLACE LE COMM LOCAL PAR LE COMM WORLD
            call asmpi_info(mpicou, rangl)
            call asmpi_comm('SET', mpicow)
            call mpicm1('BARRIER', k1bid, ibid, ibid, ibid,&
                        rbid, cbid)
            call asmpi_info(mpicow, rang, nbproc)
!         --- BUFFER DE COM K24C
!         --- K24C(FREQUENCE_COURANTE)=NBFMIN OU MAX
            k24par='&&OP0032.COULEUR'
            call jeveuo(k24par, 'L', jkpar)
            k24c='&&VPFOPR.BUFFERMPI'
            nbrow=zi(jkpar+nbproc-1)
            frecou=zi(jkpar+rang)
            call wkvect(k24c, 'V V I', nbrow+1, jk24c)
            call vecint(nbrow+1, 0, zi(jk24c))
            if (option .eq. 'STURML1P') then
                ASSERT(frecou .eq. 1)
                if (rangl .eq. 0) zi(jk24c+1)=nbfmax
!
            else if (option.eq.'STURMLNP') then
                ASSERT(frecou .gt. 1)
                if (rangl .eq. 0) zi(jk24c+frecou)=nbfmax
!
            else if (option.eq.'STURML10') then
                ASSERT(frecou .eq. 0)
                if (rangl .eq. 0) zi(jk24c)=nbfmin
!
            else if (option.eq.'STURML11') then
                ASSERT(frecou .eq. 1)
                if (rangl .eq. 0) zi(jk24c+1)=nbfmax
            endif
!
            call mpicm1('MPI_SUM', 'I', nbrow+1, ibid, zi(jk24c),&
                        rbid, cbid)
!
            if (option .eq. 'STURMLNP') then
                nbfmin=zi(jk24c+frecou-1)
                else if ((option.eq.'STURML10').or.(option.eq.'STURML11'))&
            then
                nbfmin=zi(jk24c)
                nbfmax=zi(jk24c+1)
            endif
            call jedetr(k24c)
        endif
!
        k8bid=' '
        if ((option.eq.'BANDE') .or. (option.eq.'STURMAD')) then
            typep='R'
        else if (option.eq.'STURM') then
            typep='S'
            else if ((option(1:7).eq.'STURML1').or. (option(1:7)&
        .eq.'STURMLN')) then
            typep='D'
            nbfreq=ibande
        else if (option.eq.'BANDEA') then
            typep='F'
        endif
!
        call vpecst(ifm, typres, omgmin, omgmax, nbfmin,&
                    nbfmax, nbfreq, nblagr, typep, k8bid,&
                    0.d0, dcmplx(0.d0, 0.d0))
!
!     ------------------------------------------------------------------
!     ---------INFO_MODE OU MACRO_MODE_MECA PARALLELE (PART II) --------
!     ------------------------------------------------------------------
!     --- SEULS CERTAINS PROCS REMONTENT LES OUTPUTS SINON LA COMM
!     --- EN FIN DE OP0032 VA CUMULER DES INFOS REDONDANTES.
        if ((option.eq.'STURML1P') .or. (option.eq.'STURMLNP') .or. (option.eq.'STURML10')&
            .or. (option.eq.'STURML11')) then
            if (rangl .ne. 0) then
                omemin=0.d0
                omemax=0.d0
                nbfreq=0
            endif
            if (option .eq. 'STURML11') then
                omemin=0.d0
                nbfreq=0
            else if (option.eq.'STURML10') then
                omemax=0.d0
            endif
        endif
!
        omgshi=(omgmax+omgmin)*0.5d0
        if (option(1:5) .eq. 'BANDE') then
!          --- CENTRAGE DE L INTERVALLE ---
            nbessa=0
            prec=precsh
23          continue
            ier=0
            call vpstur(lraide, omgshi, lmasse, ldynam, rbid,&
                        ibid, npivot( 1), ier, solveu, .false.,&
                        .true.)
            if (ier .ne. 0) then
                nbessa=nbessa+1
                if (nbessa .le. nbrssa) then
                    if (abs(omgshi) .lt. omecor) then
                        omgshi=omecor
                        if (ldyna) then
                            valr(1)=freqom(omgshi)
                        else
                            valr(1)=omgshi
                        endif
                        if (niv .ge. 1) call u2mesr('I', 'ALGELINE6_44', 1, valr)
! --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                        nbessa=nbrssa
                    else
                        omgdec=max(omecor,prec*abs(omgshi))
                        omgshi=omgshi-omgdec
                        if (ldyna) then
                            valr(1)=freqom(omgshi)
                        else
                            valr(1)=omgshi
                        endif
                        if (niv .ge. 1) then
                            valr(2)=prec*100.d0
                            call u2mesr('I', 'ALGELINE6_92', 2, valr)
                        endif
                        prec=2.d0*prec
                    endif
                    goto 23
                else
                    if (ldyna) then
                        valr(1)=freqom(omgshi)
                    else
                        valr(1)=omgshi
                    endif
                    call u2mesg('F', 'ALGELINE3_81', 3, valk, 0,&
                                0, 1, valr)
                endif
            endif
        endif
        omeshi=omgshi
!
!          --- AFFICHAGE COMMUN ---
        if ((niv.ge.1) .and. (option(1:5).eq.'BANDE')) then
            if (ldyna) then
                valr(1)=freqom(omgmin)
                valr(2)=freqom(omgmax)
                call u2mesr('I', 'ALGELINE6_93', 2, valr)
                if (option(1:5) .eq. 'BANDE') call u2mesr('I', 'ALGELINE6_42', 1, freqom(omeshi))
            else
                valr(1)=omgmin
                valr(2)=omgmax
                call u2mesr('I', 'ALGELINE6_94', 2, valr)
                if (option(1:5) .eq. 'BANDE') call u2mesr('I', 'ALGELINE6_43', 1, omeshi)
            endif
        endif
!
!
!     ------------------------------------------------------------------
!     ------------------------ OPTION PLUS_PETITE OU TOUT -------------
!     ------------------------------------------------------------------
!
    else if ((option.eq.'PLUS_PETITE').or.(option.eq.'TOUT')) then
!
        omgshi = 0.d0
        nbessa = 0
        prec=precsh
30      continue
        ier=0
        call vpstur(lraide, omgshi, lmasse, ldynam, rbid,&
                    ibid, npivot(1), ier, solveu, .false.,&
                    .true.)
        if (ier .ne. 0) then
            nbessa=nbessa+1
            if (nbessa .le. nbrssa) then
                if (abs(omgshi) .lt. omecor) then
                    omgshi=-omecor
                    if (ldyna) then
                        valr(1)=freqom(omgshi)
                    else
                        valr(1)=omgshi
                    endif
                    if (niv .ge. 1) call u2mesr('I', 'ALGELINE6_48', 1, valr)
! --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                    nbessa=nbrssa
                else
                    omgdec=max(omecor,prec*abs(omgshi))
                    omgshi=omgshi-omgdec
                    if (ldyna) then
                        valr(1)=freqom(omgshi)
                    else
                        valr(1)=omgshi
                    endif
                    if (niv .ge. 1) then
                        valr(2)=prec*100.d0
                        call u2mesr('I', 'ALGELINE6_92', 2, valr)
                    endif
                    prec=2.d0*prec
                endif
                goto 30
            else
                if (ldyna) then
                    valr(1)=freqom(omgshi)
                else
                    valr(1)=omgshi
                endif
                call u2mesg('F', 'ALGELINE3_81', 3, valk, 0,&
                            0, 1, valr)
            endif
        endif
        omeshi=omgshi
        if (niv .ge. 1) then
            if (ldyna) then
                call u2mesr('I', 'ALGELINE6_42', 1, freqom(omeshi))
            else
                call u2mesr('I', 'ALGELINE6_43', 1, omeshi)
            endif
        endif
!
!     ------------------------------------------------------------------
!     ------------------------ OPTION NON CONNUE -----------------------
!     ------------------------------------------------------------------
!
    else
        ch16=option
        call u2mesk('F', 'ALGELINE3_69', 1, ch16)
    endif
!
    if ((niv.ge.1) .and. (option(1:5).ne.'STURM')) write(ifm,1200)
!
!     -----------------------------FORMAT------------------------------
!
    1200 format (72('-'),/)
!     ------------------------------------------------------------------
!
end subroutine
