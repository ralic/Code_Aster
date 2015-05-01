subroutine poibij(npv, vabs, geom, fsvr, nbm,&
                  vicoq, torco, tcoef, freq, imasse,&
                  maj, vecpr)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! COUPLAGE FLUIDELASTIQUE, CONFIGURATIONS DU TYPE "COQUE_COAX"
! CALCUL D'UN CRITERE DE POIDS DES TERMES EXTRADIAGONAUX DE LA MATRICE
! B(S) PAR RAPPORT AUX TERMES DIAGONAUX
! IMPRESSION DU CRITERE DANS LE FICHIER MESSAGE
! APPELANT : FLUST4
!-----------------------------------------------------------------------
!  IN : NPV    : NOMBRE DE VITESSES D'ECOULEMENT
!  IN : VABS   : VECTEUR DES VALEURS ABSOLUES DES VITESSES D'ECOULEMENT
!                (VITESSES DE L'ECOULEMENT MOYEN)
!  IN : GEOM   : VECTEUR DE GRANDEURS GEOMETRIQUES CARACTERISTIQUES
!  IN : FSVR   : OBJET .FSVR DU CONCEPT TYPE_FLUI_STRU
!  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
!  IN : VICOQ  : VECTEUR D'INDICES CARACTERISANT LA COQUE EN MOUVEMENT
!                POUR CHAQUE MODE
!                VICOQ(IMOD)=1 COQUE INTERNE EN MVT POUR LE MODE IMOD
!                VICOQ(IMOD)=2 COQUE EXTERNE EN MVT
!                VICOQ(IMOD)=3 COQUES INTERNE + EXTERNE EN MVT
!  IN : TORCO  : TABLEAU DES ORDRES DE COQUE ET DEPHASAGES
!  IN : TCOEF  : TABLEAU DES COEFFICIENTS DES DEFORMEES AXIALES
!  IN : FREQ   : LISTE DES FREQUENCES ET AMORTISSEMENTS REDUITS MODAUX
!                PERTURBES PAR L'ECOULEMENT
!  IN : IMASSE : INDICE CARACTERISTIQUE DU CAS DE CALCUL
!                IMASSE=0  EFFETS DE MASSE AJOUTEE NON PRIS EN COMPTE
!                IMASSE=1  EFFETS DE MASSE AJOUTEE PRIS EN COMPTE
!  IN : MAJ    : MASSES AJOUTEES PAR LE FLUIDE (DANS LA BASE EN EAU)
!  IN : VECPR  : SI IMASSE=0 : INUTILE
!                SI IMASSE=1 : VECTEURS PROPRES DES MODES EN EAU
!                              DECOMPOSES SUR LA BASE DES MODES EN AIR
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/bmocca.h"
#include "asterfort/cfrott.h"
#include "asterfort/cripoi.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    integer :: npv
    real(kind=8) :: vabs(npv), geom(9), fsvr(7)
    integer :: nbm, vicoq(nbm)
    real(kind=8) :: torco(4, nbm), tcoef(10, nbm), freq(2*nbm*npv)
    integer :: imasse
    real(kind=8) :: maj(nbm), vecpr(*)
!
    real(kind=8) :: mcf0, ksi
    complex(kind=8) :: s
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ifm, imat2, imatb, imod, iv
    integer :: j, k
    real(kind=8) :: cf0, crit, fi, hmoy, omegai, pi, rug
    real(kind=8) :: s1, s2, umoy, visc
!-----------------------------------------------------------------------
    call jemarq()
!
!
!-----1.INITIALISATIONS ET FORMATS D'IMPRESSION
!
    pi = r8pi()
    hmoy = geom(1)
    visc = fsvr(2)
    rug = fsvr(3)
!
    ifm = iunifi('MESSAGE')
!
    500 format('********************************************************')
    501 format('*                                                      *')
    502 format('* MATRICE DE TRANSFERT DES FORCES FLUIDELASTIQUES B(S) *')
    503 format('*         DIMINUEE DES TERMES DE MASSE AJOUTEE         *')
    504 format('*  CALCUL DU POIDS RELATIF DES TERMES EXTRADIAGONAUX   *')
    505 format('*           PAR RAPPORT AUX TERMES DIAGONAUX           *')
    510 format('VITESSE D ECOULEMENT NO ',i3)
    511 format('===========================')
    512 format('VITESSE D ECOULEMENT NULLE : LA MATRICE DE TRANSFERT ',&
     &       'DES FORCES FLUIDELASTIQUES')
    513 format('EST EGALE A LA MATRICE DE MASSE AJOUTEE. TOUS LES ',&
     &       'TERMES RESIDUELS SONT NULS')
    514 format('N.B. TOUS LES TERMES DE LA MATRICE DE MASSE AJOUTEE ',&
     &       'ONT ETE PRIS EN COMPTE POUR')
    515 format('LE CALCUL DES MODES DE LA STRUCTURE EN EAU AU REPOS')
    520 format('FREQUENCE NO ',i3,' NON DETERMINEE')
    521 format('FREQUENCE SOLUTION NO ',i3,3x,'POIDS RELATIF DES BIJ : ',&
     &        g13.6,' %')
    530 format(30x,'---/---')
!
!
!-----2.CALCUL DU CRITERE
!
    call wkvect('&&POIBIJ.TEMP.MATB', 'V V C', nbm*nbm, imatb)
!
    write(ifm,500)
    write(ifm,501)
    write(ifm,502)
    write(ifm,503)
    write(ifm,501)
    write(ifm,504)
    write(ifm,505)
    write(ifm,501)
    write(ifm,500)
    write(ifm,*)
!
!-----2.1.SI ON TRAVAILLE DIRECTEMENT DANS LA BASE MODALE EN EAU AU
!         REPOS DU SYSTEME : LES TERMES RESIDUELS SONT DONNES PAR
!                                           2
!                      R(S) = B(S) + MAJ * S
!
    if (imasse .eq. 0) then
!
        do 10 iv = 1, npv
!
            write(ifm,510) iv
            write(ifm,511)
            write(ifm,*)
            umoy = vabs(iv)
!
!---------2.1.1.CAS VITESSE NULLE : B(S) = -MAJ => TERMES RESIDUELS NULS
!
            if (umoy .lt. 1.d-5) then
!
                write(ifm,512)
                write(ifm,513)
                write(ifm,*)
                write(ifm,514)
                write(ifm,515)
!
!---------2.1.2.CAS VITESSE NON NULLE
!
            else
!
                call cfrott(visc, rug, hmoy, umoy, cf0,&
                            mcf0)
!
                do 20 imod = 1, nbm
!
                    fi = freq(2*nbm*(iv-1)+2*(imod-1)+1)
                    ksi = freq(2*nbm*(iv-1)+2*(imod-1)+2)
!
                    if (fi .lt. 0.d0 .or. ksi .gt. 1.d0) then
!
                        write(ifm,520) imod
!
                    else
!
                        omegai = 2.d0*pi*fi
                        s1 = -1.d0*omegai*ksi
                        s2 = omegai*dble(sqrt(1.d0-ksi*ksi))
                        s = dcmplx(s1,s2)
!
                        call bmocca(umoy, geom, cf0, mcf0, fsvr,&
                                    nbm, vicoq, torco, tcoef, s1,&
                                    s2, zc(imatb))
!                                     2
!---------------B(S) -> B(S) + MAJ * S
!
                        do 30 j = 1, nbm
                            zc(imatb+nbm*(j-1)+j-1) = zc(&
                                                      imatb+nbm*(j- 1)+j-1) + dcmplx(maj(j), 0.d0&
                                                      )*s*s
30                      continue
!
                        call cripoi(nbm, zc(imatb), crit)
                        write(ifm,521) imod,crit
!
                    endif
!
20              continue
!
            endif
!
            write(ifm,*)
            write(ifm,530)
            write(ifm,*)
!
10      continue
!
!-----2.2.SINON : ON DOIT PROJETER B(S) SUR LA BASE EN EAU AU REPOS DU
!         SYSTEME. LES TERMES RESIDUELS SONT DONNES PAR
!                        T                          2
!                 R(S) =  PHI * B(S) * PHI + MAJ * S
!
    else
!
        call wkvect('&&POIBIJ.TEMP.MAT2', 'V V C', nbm*nbm, imat2)
!
        do 110 iv = 1, npv
!
            write(ifm,510) iv
            write(ifm,511)
            write(ifm,*)
            umoy = vabs(iv)
!                                                       T
!---------2.2.1.CAS VITESSE NULLE : B(S) = - PHI * MAJ * PHI
!               => TERMES RESIDUELS NULS
!
            if (umoy .lt. 1.d-5) then
!
                write(ifm,512)
                write(ifm,513)
                write(ifm,*)
                write(ifm,514)
                write(ifm,515)
!
!---------2.2.2.CAS VITESSE NON NULLE
!
            else
!
                call cfrott(visc, rug, hmoy, umoy, cf0,&
                            mcf0)
!
                do 120 imod = 1, nbm
!
                    fi = freq(2*nbm*(iv-1)+2*(imod-1)+1)
                    ksi = freq(2*nbm*(iv-1)+2*(imod-1)+2)
!
                    if (fi .lt. 0.d0 .or. ksi .gt. 1.d0) then
!
                        write(ifm,520) imod
!
                    else
!
                        omegai = 2.d0*pi*fi
                        s1 = -1.d0*omegai*ksi
                        s2 = omegai*dble(sqrt(1.d0-ksi*ksi))
                        s = dcmplx(s1,s2)
!
                        call bmocca(umoy, geom, cf0, mcf0, fsvr,&
                                    nbm, vicoq, torco, tcoef, s1,&
                                    s2, zc(imatb))
!                *
!---------------B (S) = B(S) * PHI
!
                        do 130 j = 1, nbm
                            do 131 i = 1, nbm
                                zc(imat2+nbm*(j-1)+i-1) = dcmplx(0.d0, 0.d0)
                                do 132 k = 1, nbm
                                    zc(imat2+nbm*(j-1)+i-1) = zc(&
                                                              imat2+nbm*(j-1)+i-1) + zc(imatb+ nb&
                                                              &m*(k-1)+i-1) * dcmplx(vecpr(nbm* (&
                                                              &j-1)+k),&
                                                              0.d0&
                                                              )
132                              continue
131                          continue
130                      continue
!                       T       *
!---------------B(S) ->  PHI * B (S)
!
                        do 140 j = 1, nbm
                            do 141 i = 1, nbm
                                zc(imatb+nbm*(j-1)+i-1) = dcmplx(0.d0, 0.d0)
                                do 142 k = 1, nbm
                                    zc(imatb+nbm*(j-1)+i-1) = zc(&
                                                              imatb+nbm*(j-1)+i-1) + dcmplx( vecp&
                                                              &r(nbm*(i-1)+k),&
                                                              0.d0) * zc(imat2+nbm*(j-1)+k-1&
                                                              )
142                              continue
141                          continue
140                      continue
!                                     2
!---------------B(S) -> B(S) + MAJ * S
!
                        do 150 j = 1, nbm
                            zc(imatb+nbm*(j-1)+j-1) = zc(&
                                                      imatb+nbm*(j- 1)+j-1) + dcmplx(maj(j), 0.d0&
                                                      )*s*s
150                      continue
!
                        call cripoi(nbm, zc(imatb), crit)
                        write(ifm,521) imod,crit
!
                    endif
!
120              continue
!
            endif
!
            write(ifm,*)
            write(ifm,530)
            write(ifm,*)
!
110      continue
!
    endif
!
! --- MENAGE
    call jedetr('&&POIBIJ.TEMP.MATB')
    call jedetr('&&POIBIJ.TEMP.MAT2')
!
    call jedema()
end subroutine
