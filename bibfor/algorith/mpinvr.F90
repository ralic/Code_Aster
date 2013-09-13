subroutine mpinvr(nbmesu, nbmode, nbabs, phi, rmesu,&
                  coef, xabs, lfonct, reta)
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
!     PROJ_MESU_MODAL : RESOLUTION DU SYSTEME PAR SVD OU PAR LU
!                       (DONNEES FREQUENTIELLES REELLES)
!
!     IN  : NBMESU : NOMBRE DE MESURE
!     IN  : NBMODE : NOMBRE DE MODES
!     IN  : NBABS  : NOMBRE D ABSCISSES
!     IN  : PHI    : BASE DE PROJECTION
!     IN  : RMESU  : VALEURS MESUREES
!     IN  : COEF   : COEFFICIENTS DE PONDERATION
!     IN  : XABS   : LISTE REELLE D ABSCISSES
!     IN  : LFONCT : .TRUE. SI COEFFICIENTS DE PONDERATION
!                    DEPENDENT DE LA FREQUENCE
!     OUT : RETA   : DEPLACEMENT GENERALISE   ( MATRICE )
!
    implicit none
!
!
!
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/mtcrog.h"
#include "asterfort/rslsvd.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
    integer :: nbmesu, nbmode, nbabs
    integer :: vali
    real(kind=8) :: phi(nbmesu, nbmode), xabs(nbabs), coef(*)
    real(kind=8) :: valr
    real(kind=8) :: rmesu(nbmesu, nbabs), reta(nbmode, nbabs)
    logical :: lfonct
!
    integer :: imod, jmod, imes, iabs, ierr, ibid, jmes
    integer :: lsecmb, lwks, lphiph, lphitp, lmatsy, lwork, leta, lvals, lu, lv
    real(kind=8) :: alpha, eps
    logical :: nul
    character(len=3) :: method
    character(len=8) :: regul
    character(len=16) :: nomcha
    character(len=16) :: secmb, wks, phipht, phitph, matsys, work, eta, vals, u
    character(len=16) :: v
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! CREATION DES VECTEURS DE TRAVAIL
    secmb = '&SECMB'
    wks = '&WKS'
    phipht = '&PHIPHT'
    phitph = '&PHITPH'
    matsys = '&MATSYS'
    work = '&WORK'
    eta = '&ETA'
    vals = '&VALS'
    u = '&U'
    v = '&V'
!
    call wkvect(secmb, 'V V R', nbmode, lsecmb)
    call wkvect(wks, 'V V R', nbmode, lwks)
    call wkvect(phipht, 'V V R', nbmesu*nbmesu, lphiph)
    call wkvect(phitph, 'V V R', nbmode*nbmode, lphitp)
    call wkvect(matsys, 'V V R', nbmode*nbmode, lmatsy)
    call wkvect(work, 'V V R', nbmode, lwork)
    call wkvect(eta, 'V V R', nbmode, leta)
    call wkvect(vals, 'V V R', nbmode, lvals)
    call wkvect(u, 'V V R', nbmode*nbmode, lu)
    call wkvect(v, 'V V R', nbmode*nbmode, lv)
!
! METHODE DE RESOLUTION : LU / SVD
    call getvtx('RESOLUTION', 'METHODE', iocc=1, scal=method, nbret=ibid)
    if (ibid .eq. 0) method = 'LU'
!
    if (method .eq. 'SVD') then
        call getvr8('RESOLUTION', 'EPS', iocc=1, scal=eps, nbret=ibid)
        if (ibid .eq. 0) eps = 0.d0
    endif
!
! REGULARISATION : NON / NORM_MIN / TIK_RELA
    call getvtx('RESOLUTION', 'REGUL', iocc=1, scal=regul, nbret=ibid)
    if (ibid .eq. 0) regul = 'NON'
!
    call getvtx('MODELE_MESURE', 'NOM_CHAM', iocc=1, scal=nomcha, nbret=ibid)
!
! ===============================
! CALCUL DE PHI_TRANSPOSEE * PHI
! ===============================
    do 30 imod = 1, nbmode
        do 20 jmod = 1, nbmode
            zr(lphitp-1 +imod+nbmode*(jmod-1)) = 0.d0
            do 10 imes = 1, nbmesu
                zr(lphitp-1 +imod+nbmode*(jmod-1)) = zr(&
                                                     lphitp-1 + imod+nbmode*(jmod-1)) + phi(imes,&
                                                     imod)*phi(imes, jmod&
                                                     )
10          continue
20      continue
30  end do
!
    if (nbmesu .lt. nbmode) then
! ===============================
! CALCUL DE PHI * PHI_TRANSPOSEE
! ===============================
        do 40 imes = 1, nbmesu
            do 50 jmes = 1, nbmesu
                zr(lphiph-1 +imes+nbmesu*(jmes-1)) = 0.d0
                do 60 imod = 1, nbmode
                    zr(lphiph-1 +imes+nbmesu*(jmes-1)) = zr(&
                                                         lphiph-1 + imes+nbmesu*(jmes-1)&
                                                         ) + phi(imes,&
                                                         imod)*phi(jmes, imod&
                                                         )
60              continue
50          continue
40      continue
    endif
!
! =======================================
! CALCUL DE LA REPONSE GENERALISEE : RETA
! =======================================
!
! DEBUT DE LA BOUCLE SUR LES ABSCISSES (FREQUENCE)
! *****************************
    do 100 iabs = 1, nbabs
!
        nul = .true.
!
! DEBUT DE LA BOUCLE SUR LES MODES
! ********************************
        do 90 imod = 1, nbmode
!
! RECHERCHE DU COEFFICIENT DE PONDERATION
! ***************************************
            if (lfonct) then
!             -> ALPHA DEPENDANT DES ABSCISSES
                alpha = coef (nbmode*(iabs-1)+imod)
            else
!             -> ALPHA INDEPENDANT DES ABSCISSES
                alpha = coef (imod)
            endif
!
!         -> ON VERIFIE QUE ALPHA > 0, SINON ARRET
            if (alpha .lt. 0.d0) then
                vali = iabs
                call u2mesg('F', 'ALGORITH15_24', 0, ' ', 1,&
                            vali, 0, 0.d0)
            else if (alpha .gt. r8prem()) then
                nul=.false.
            endif
!
! DETERMINATION DE LA MATRICE A INVERSER :
! MATSYS(IABS) = PHI_T*PHI + ALPHA(IABS)
! ****************************************
            do 80 jmod = 1, nbmode
                zr(lmatsy-1 +imod+nbmode*(jmod-1)) = zr( lphitp-1 + imod+nbmode*( jmod-1) )
80          continue
!
            zr(lmatsy-1 +imod+nbmode*(imod-1)) = zr( lmatsy-1 +imod+ nbmode*(imod-1 ) ) + alpha
!
! DETERMINATION DU SECOND MEMBRE :
! SCDMB(IABS) = PHI_T*Q + ALPHA(IABS)*RETA(IABS-1)
! RQ : A IABS=1, RETA(0)=0 (LA SOLUTION A PRIORI EST NULLE)
! ********************************************************
            zr(lsecmb-1 +imod) = 0.d0
!
            do 70 imes = 1, nbmesu
                zr(lsecmb-1 +imod) = zr(lsecmb-1 +imod) + phi(imes, imod)*rmesu(imes,iabs)
70          continue
!
            if ((regul .eq. 'TIK_RELA') .and. (iabs .gt. 1)) then
                zr(lsecmb-1 +imod) = zr(lsecmb-1 +imod) + alpha*reta( imod,iabs-1)
            endif
!
! FIN DE LA BOUCLE SUR LES MODES
! ******************************
90      continue
!
!
! RESOLUTION DU SYSTEME :
! MATSYS(IABS) * ETA(IABS) = SCDMB(IABS)
! **************************************
!       -> ALARME SI ALPHA NUL ET NBMESU<NBMODE : MOINDRE NORME
        if ((nbmesu .lt. nbmode) .and. (nul)) then
            call u2mesg('A', 'ALGORITH15_25', 0, ' ', 0,&
                        0, 0, 0.d0)
!
            if (regul .eq. 'NON') then
! CALCUL MOINDRE NORME
                call u2mesg('A', 'ALGORITH15_26', 0, ' ', 0,&
                            0, 0, 0.d0)
                do 71 imes = 1, nbmesu
                    zr(lsecmb-1 +imes) = rmesu(imes,iabs)
                    do 77 jmes = 1, nbmesu
                        zr(lmatsy-1 +imes+nbmode*(jmes-1)) = zr(lphiph-1 +imes+nbmesu*( jmes-1))
77                  continue
71              continue
!
! CHOIX POUR LA METHODE D INVERSION
                if (method .eq. 'SVD') then
! METHODE SVD
! CREATION DU VECTEUR SECOND MEMBRE
                    do 75 jmes = 1, nbmesu
                        zr(leta-1 +jmes) = zr(lsecmb-1 +jmes)
75                  continue
!
                    call rslsvd(nbmode, nbmesu, nbmesu, zr(lmatsy), zr( lvals),&
                                zr(lu), zr(lv), 1, zr(leta), eps,&
                                ierr, zr( lwork))
                    if (ierr .ne. 0) then
                        vali = iabs
                        valr = xabs ( iabs )
                        call u2mesg('F', 'ALGORITH15_27', 0, ' ', 1,&
                                    vali, 1, valr)
                    endif
!
                else
! METHODE DE CROUT
                    call mtcrog(zr(lmatsy), zr(lsecmb), nbmode, nbmesu, 1,&
                                zr(leta), zr(lwks), ierr)
                    if (ierr .ne. 0) then
                        vali = iabs
                        valr = xabs ( iabs )
                        call u2mesg('F', 'ALGORITH15_28', 0, ' ', 1,&
                                    vali, 1, valr)
                    endif
                endif
!
! COPIE DES RESULTATS DANS RETA
                do 76 jmod = 1, nbmode
                    do 74 jmes = 1, nbmesu
                        reta(jmod,iabs) = phi(jmes,jmod)*zr(leta-1 + jmes)
74                  continue
76              continue
!
                goto 100
            endif
        endif
! FIN CALCUL MOINDRE NORME
!
!
! RESOLUTION EN FONCTION DE LA METHODE DE RESOLUTION
        if (method .eq. 'SVD') then
! METHODE SVD
! CREATION DU VECTEUR SECOND MEMBRE
            do 81 jmod = 1, nbmode
                zr(leta-1 +jmod) = zr(lsecmb-1 +jmod)
81          continue
!
            call rslsvd(nbmode, nbmode, nbmode, zr(lmatsy), zr( lvals),&
                        zr(lu), zr(lv), 1, zr(leta), eps,&
                        ierr, zr(lwork))
            if (ierr .ne. 0) then
                vali = iabs
                valr = xabs ( iabs )
                call u2mesg('F', 'ALGORITH15_27', 0, ' ', 1,&
                            vali, 1, valr)
            endif
!
        else
! METHODE DE CROUT
            call mtcrog(zr(lmatsy), zr(lsecmb), nbmode, nbmode, 1,&
                        zr(leta), zr(lwks), ierr)
            if (ierr .ne. 0) then
                vali = iabs
                valr = xabs ( iabs )
                call u2mesg('F', 'ALGORITH15_28', 0, ' ', 1,&
                            vali, 1, valr)
            endif
        endif
!
! RECUPERATION DES RESULTATS
        do 73 jmod = 1, nbmode
            reta(jmod,iabs)=zr(leta-1+jmod)
73      continue
!
! FIN DE LA BOUCLE SUR LES ABSCISSES (FREQUENCE)
! ***************************
100  end do
!
!
! DESTRUCTION DES VECTEURS DE TRAVAIL
!
    call jedetr(secmb)
    call jedetr(wks)
    call jedetr(phipht)
    call jedetr(phitph)
    call jedetr(matsys)
    call jedetr(work)
    call jedetr(eta)
    call jedetr(vals)
    call jedetr(u)
    call jedetr(v)
!
    call jedema()
end subroutine
