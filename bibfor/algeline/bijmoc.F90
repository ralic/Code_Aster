subroutine bijmoc(umoy, geom, cf0, mcf0, fsvr,&
                  imod, jmod, nbm, vicoq, torco,&
                  tcoef, s1, s2, bij)
    implicit none
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
!-----------------------------------------------------------------------
! COUPLAGE FLUIDELASTIQUE, CONFIGURATIONS DU TYPE "COQUE_COAX"
! CALCUL DU TERME (I,J) DE LA MATRICE DE TRANSFERT DES FORCES
! FLUIDELASTIQUES PROJETEE SUR LA BASE MODALE DES STRUCTURES
! ROUTINE CHAPEAU
! APPELANT : FLUST4, PACOUF
!-----------------------------------------------------------------------
!  IN : UMOY   : VITESSE DE L'ECOULEMENT MOYEN
!  IN : GEOM   : VECTEUR DE GRANDEURS GEOMETRIQUES CARACTERISTIQUES
!  IN : CF0    : COEFFICIENT DE FROTTEMENT VISQUEUX
!  IN : MCF0   : EXPOSANT VIS-A-VIS DU NOMBRE DE REYNOLDS
!  IN : FSVR   : OBJET .FSVR DU CONCEPT TYPE_FLUI_STRU
!  IN : IMOD   : INDICE DU MODE CONSIDERE POUR LE PROBLEME FLUIDE
!  IN : JMOD   : INDICE DU SECOND MODE CONSIDERE
!  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
!  IN : VICOQ  : VECTEUR D'INDICES CARACTERISANT LA COQUE EN MOUVEMENT
!                POUR CHAQUE MODE
!                VICOQ(IMOD)=1 COQUE INTERNE EN MVT POUR LE MODE IMOD
!                VICOQ(IMOD)=2 COQUE EXTERNE EN MVT
!                VICOQ(IMOD)=3 COQUES INTERNE + EXTERNE EN MVT
!  IN : TORCO  : TABLEAU DES ORDRES DE COQUE ET DEPHASAGES
!  IN : TCOEF  : TABLEAU DES COEFFICIENTS DES DEFORMEES AXIALES
!  IN : S1     : PARTIE REELLE DE LA FREQUENCE COMPLEXE A LAQUELLE ON
!                CALCULE LE TERME (I,J) DE LA MATRICE B
!  IN : S2     : PARTIE IMAGINAIRE DE LA FREQUENCE COMPLEXE
! OUT : BIJ    : TERME (I,J) DE LA MATRICE B, COMPLEXE
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/bijsom.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/pbflui.h"
#include "asterfort/wkvect.h"
    real(kind=8) :: umoy, geom(9), cf0, mcf0, fsvr(7)
    integer :: imod, jmod, nbm, vicoq(nbm)
    real(kind=8) :: torco(4, nbm), tcoef(10, nbm), s1, s2
    complex(kind=8) :: bij
!
    real(kind=8) :: long
    complex(kind=8) :: bij1, bij2, bij11, bij21, bij12, bij22
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: icoq, iligne, iysol, iysol1, iysol2, jcoq, jligne
    integer :: k1i, k1j, k2i, k2j, ki, kj
    real(kind=8) :: hmoy, r1, r2, rhof, rk1i, rk1j, rk2i
    real(kind=8) :: rk2j, rki, rkj, rmoy, thet1i, thet1j, thet2i
    real(kind=8) :: thet2j, thetai, thetaj
!-----------------------------------------------------------------------
    call jemarq()
!
!
! --- 1.INITIALISATIONS ET CREATION DE VECTEURS DE TRAVAIL
!
    hmoy = geom(1)
    rmoy = geom(2)
    long = geom(3)
!
    r1 = rmoy - hmoy/2.d0
    r2 = rmoy + hmoy/2.d0
!
    rhof = fsvr(1)
!
    call wkvect('&&BIJMOC.TEMP.YSOL', 'V V C', 3*101, iysol)
    call wkvect('&&BIJMOC.TEMP.YSOL1', 'V V C', 3*101, iysol1)
    call wkvect('&&BIJMOC.TEMP.YSOL2', 'V V C', 3*101, iysol2)
!
!
! --- 2.CALCUL DE BIJ
!
    icoq = vicoq(imod)
!
!-----SI UNE SEULE COQUE EST EN MOUVEMENT POUR LE MODE IMOD
!
    if (icoq .lt. 3) then
!
        iligne = 1
        if (icoq .eq. 2) iligne = 3
        rki = torco(iligne,imod)
        ki = int(rki)
        thetai = torco(iligne+1,imod)
!
        jcoq = vicoq(jmod)
!
!-------SI UNE SEULE COQUE EST EN MOUVEMENT POUR LE MODE JMOD
!
        if (jcoq .lt. 3) then
!
            jligne = 1
            if (jcoq .eq. 2) jligne = 3
            rkj = torco(jligne,jmod)
            kj = int(rkj)
            thetaj = torco(jligne+1,jmod)
!
            if (ki .eq. kj) then
                call pbflui(umoy, hmoy, rmoy, long, cf0,&
                            mcf0, fsvr, icoq, imod, nbm,&
                            rki, tcoef, s1, s2, zc(iysol))
                call bijsom(umoy, rhof, r1, r2, long,&
                            cf0, icoq, jcoq, jmod, nbm,&
                            rki, thetai, thetaj, tcoef, zc(iysol),&
                            bij)
            else
                bij = dcmplx(0.d0,0.d0)
            endif
!
!-------SINON (LES DEUX COQUES SONT EN MOUVEMENT POUR LE MODE JMOD)
!
        else
!
            rk1j = torco(1,jmod)
            k1j = int(rk1j)
            thet1j = torco(2,jmod)
!
            rk2j = torco(3,jmod)
            k2j = int(rk2j)
            thet2j = torco(4,jmod)
!
            if (ki .eq. k1j .or. ki .eq. k2j) then
!
                call pbflui(umoy, hmoy, rmoy, long, cf0,&
                            mcf0, fsvr, icoq, imod, nbm,&
                            rki, tcoef, s1, s2, zc(iysol))
                if (ki .eq. k1j) then
                    call bijsom(umoy, rhof, r1, r2, long,&
                                cf0, icoq, 1, jmod, nbm,&
                                rki, thetai, thet1j, tcoef, zc(iysol),&
                                bij1)
                else
                    bij1 = dcmplx(0.d0,0.d0)
                endif
                if (ki .eq. k2j) then
                    call bijsom(umoy, rhof, r1, r2, long,&
                                cf0, icoq, 2, jmod, nbm,&
                                rki, thetai, thet2j, tcoef, zc(iysol),&
                                bij2)
                else
                    bij2 = dcmplx(0.d0,0.d0)
                endif
                bij = bij1 + bij2
!
            else
!
                bij = dcmplx(0.d0,0.d0)
!
            endif
!
        endif
!
!-----SINON (LES DEUX COQUES SONT EN MOUVEMENT POUR LE MODE IMOD)
!
    else
!
        rk1i = torco(1,imod)
        k1i = int(rk1i)
        thet1i = torco(2,imod)
!
        rk2i = torco(3,imod)
        k2i = int(rk2i)
        thet2i = torco(4,imod)
!
        jcoq = vicoq(jmod)
!
!-------SI UNE SEULE COQUE EST EN MOUVEMENT POUR LE MODE JMOD
!
        if (jcoq .lt. 3) then
!
            jligne = 1
            if (jcoq .eq. 2) jligne = 3
            rkj = torco(jligne,jmod)
            kj = int(rkj)
            thetaj = torco(jligne+1,jmod)
!
            if (k1i .eq. kj) then
                call pbflui(umoy, hmoy, rmoy, long, cf0,&
                            mcf0, fsvr, 1, imod, nbm,&
                            rk1i, tcoef, s1, s2, zc(iysol1))
                call bijsom(umoy, rhof, r1, r2, long,&
                            cf0, 1, jcoq, jmod, nbm,&
                            rk1i, thet1i, thetaj, tcoef, zc(iysol1),&
                            bij1)
            else
                bij1 = dcmplx(0.d0,0.d0)
            endif
!
            if (k2i .eq. kj) then
                call pbflui(umoy, hmoy, rmoy, long, cf0,&
                            mcf0, fsvr, 2, imod, nbm,&
                            rk2i, tcoef, s1, s2, zc(iysol2))
                call bijsom(umoy, rhof, r1, r2, long,&
                            cf0, 2, jcoq, jmod, nbm,&
                            rk2i, thet2i, thetaj, tcoef, zc(iysol2),&
                            bij2)
            else
                bij2 = dcmplx(0.d0,0.d0)
            endif
!
            bij = bij1 + bij2
!
!-------SINON (LES DEUX COQUES SONT EN MOUVEMENT POUR LE MODE JMOD)
!
        else
!
            rk1j = torco(1,jmod)
            k1j = int(rk1j)
            thet1j = torco(2,jmod)
!
            rk2j = torco(3,jmod)
            k2j = int(rk2j)
            thet2j = torco(4,jmod)
!
            if (k1i .eq. k1j .or. k1i .eq. k2j) then
!
                call pbflui(umoy, hmoy, rmoy, long, cf0,&
                            mcf0, fsvr, 1, imod, nbm,&
                            rk1i, tcoef, s1, s2, zc(iysol1))
                if (k1i .eq. k1j) then
                    call bijsom(umoy, rhof, r1, r2, long,&
                                cf0, 1, 1, jmod, nbm,&
                                rk1i, thet1i, thet1j, tcoef, zc(iysol1),&
                                bij11)
                else
                    bij11 = dcmplx(0.d0,0.d0)
                endif
                if (k1i .eq. k2j) then
                    call bijsom(umoy, rhof, r1, r2, long,&
                                cf0, 1, 2, jmod, nbm,&
                                rk1i, thet1i, thet2j, tcoef, zc(iysol1),&
                                bij12)
                else
                    bij12 = dcmplx(0.d0,0.d0)
                endif
                bij1 = bij11 + bij12
!
            else
!
                bij1 = dcmplx(0.d0,0.d0)
!
            endif
!
            if (k2i .eq. k1j .or. k2i .eq. k2j) then
!
                call pbflui(umoy, hmoy, rmoy, long, cf0,&
                            mcf0, fsvr, 2, imod, nbm,&
                            rk2i, tcoef, s1, s2, zc(iysol2))
                if (k2i .eq. k1j) then
                    call bijsom(umoy, rhof, r1, r2, long,&
                                cf0, 2, 1, jmod, nbm,&
                                rk2i, thet2i, thet1j, tcoef, zc(iysol2),&
                                bij21)
                else
                    bij21 = dcmplx(0.d0,0.d0)
                endif
                if (k2i .eq. k2j) then
                    call bijsom(umoy, rhof, r1, r2, long,&
                                cf0, 2, 2, jmod, nbm,&
                                rk2i, thet2i, thet2j, tcoef, zc(iysol2),&
                                bij22)
                else
                    bij22 = dcmplx(0.d0,0.d0)
                endif
                bij2 = bij21 + bij22
!
            else
!
                bij2 = dcmplx(0.d0,0.d0)
!
            endif
!
            bij = bij1 + bij2
!
        endif
!
    endif
!
    call jedetr('&&BIJMOC.TEMP.YSOL')
    call jedetr('&&BIJMOC.TEMP.YSOL1')
    call jedetr('&&BIJMOC.TEMP.YSOL2')
    call jedema()
end subroutine
