subroutine ccvrrl(nommai, modele, carael, mesmai, chames,&
                  cmperr, codret)
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
    implicit none
!     --- ARGUMENTS ---
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/angvec.h"
#include "asterfort/carces.h"
#include "asterfort/cccmcr.h"
#include "asterfort/cncinv.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvlg.h"
#include "asterfort/wkvect.h"
!
    integer :: codret
    character(len=1) :: cmperr
    character(len=8) :: nommai, modele, carael
    character(len=19) :: chames
    character(len=24) :: mesmai
! ----------------------------------------------------------------------
!  CALC_CHAMP - VERIFICATION DES REPERES LOCAUX
!  -    -       - -              -       -
! ----------------------------------------------------------------------
!
!  ROUTINE SERVANT A VERIFIER L'ORIENTATION DES REPERES LOCAUX
!    LORS DU PASSAGE ELNO -> NOEU
!
! IN  :
!   NOMMAI  K8   NOM DU MAILLAGE A VERIFIER
!   MODELE  K8   NOM DU MODELE
!   CARAEL  K8   NOM DU CARAELEM
!   MESMAI  K24  NOM DU VECTEUR CONTENANT LES MAILLES SUR LESQUELLES
!                LE CALCUL EST DEMANDE
!   CHAMES  K19  NOM DU CHAM_ELEM_S POUR LEQUEL ON VERIFIE LES REPERES
!   CMPERR  K1   COMPORTEMENT EN CAS DE PROBLEME
!     'F' : EMISSION D'UNE ERREUR <F>
!     'A' : EMISSION D'UNE ALARME POUR PREVENIR L'UTILISATEUR
!     ' ' : SILENCE => CODE RETOUR
!
! OUT :
!   CODRET  I    CODE RETOUR
!     0 SI OK
!     1 EN CAS DE PROBLEME
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
    integer :: jmai, nbma, ier, nbno, jconi1, jconi2, ima, ncmax, i
    integer :: posit, posma, numma, ima2, numma2, ino, jcesd, jcesl
    integer :: jcesv, iexori, jrepe, iepais, nbmato, jdime, jvect, idir
    integer :: jconx1, jconx2, jcoord, jcesdc, ialpha, ibeta
    integer :: jalpha, jbeta, jgamma, jcesc, jcescc, jcesdd, jceslc, jcesvc
    integer :: adcar1(3), adcar2(3)
!
    real(kind=8) :: maxtol, maxdif, tabres(1), pi, angle1, angle2
    real(kind=8) :: pgl(3, 3), vl(3), vg1(3), vg2(3), vg3(3), vg4(3)
!
    character(len=8) :: nomail
    character(len=16) :: modeli
    character(len=19) :: cnxinv, carsd, ligrmo, carcc, vecsau
    character(len=24) :: carori, carcoq
!
    logical :: llimai, lprobm
    parameter    (maxtol = 8.7266463d-2)
    parameter    (pi = 3.14159265358979d0)
!
    call jemarq()
    codret = 0
    ligrmo = modele//'.MODELE'
    call jeveuo(ligrmo//'.REPE', 'L', jrepe)
!
!
!     -- OBJETS QUI SERONT EVENTUELLEMNT CREES :
    carsd = '&&CCVRRL.CARORIEN'
    carcc = '&&CCVRRL.CARCOQUE'
    cnxinv = '&&CCVRRL.CNCINV'
    vecsau = '&&CCVRRL.VECT'
!
!
!
!     DOIT ON REDUIRE LE CALCUL SUR UNE LISTE DE MAILLES
    call jeexin(mesmai, ier)
    if (ier .ne. 0) then
        call jeveuo(mesmai, 'L', jmai)
        call jelira(mesmai, 'LONMAX', nbma)
        llimai = .true.
    else
        jmai = 1
        nbma = 0
        llimai = .false.
    endif
    call jeveuo(chames//'.CESD', 'L', jcesdd)
!
!     CONVERSION DE LA CARTE D'ORIENTATION EN UN CHAM_ELEM_S
    carori = carael//'.CARORIEN  .VALE'
    call jeexin(carori, iexori)
    if (iexori .ne. 0) then
        call carces(carael//'.CARORIEN', 'ELEM', ' ', 'V', carsd,&
                    ' ', ier)
        call jeveuo(carsd//'.CESD', 'L', jcesd)
        call jeveuo(carsd//'.CESL', 'L', jcesl)
        call jeveuo(carsd//'.CESV', 'L', jcesv)
        call jeveuo(carsd//'.CESC', 'L', jcesc)
        call jelira(carsd//'.CESC', 'LONMAX', ncmax)
        jalpha = indik8(zk8(jcesc),'ALPHA   ',1,ncmax)
        jbeta = indik8(zk8(jcesc),'BETA    ',1,ncmax)
        jgamma = indik8(zk8(jcesc),'GAMMA   ',1,ncmax)
    else
        jcesd=0
        jcesl=0
        jcesv=0
        jcesc=0
    endif
!
!     CONVERSION DE LA CARTE CARACTERISTIQUE DES COQUES
!     EN UN CHAM_ELEM_S
    carcoq = carael//'.CARCOQUE  .VALE'
    call jeexin(carcoq, iexori)
    if (iexori .ne. 0) then
        call carces(carael//'.CARCOQUE', 'ELEM', ' ', 'V', carcc,&
                    ' ', ier)
        call jeveuo(carcc//'.CESD', 'L', jcesdc)
        call jeveuo(carcc//'.CESL', 'L', jceslc)
        call jeveuo(carcc//'.CESV', 'L', jcesvc)
        call jeveuo(carcc//'.CESC', 'L', jcescc)
        call jelira(carcc//'.CESC', 'LONMAX', ncmax)
        iepais = indik8(zk8(jcescc),'EP      ',1,ncmax)
        ialpha = indik8(zk8(jcescc),'ALPHA   ',1,ncmax)
        ibeta = indik8(zk8(jcescc),'BETA    ',1,ncmax)
    else
        jcesdc=0
        jceslc=0
        jcesvc=0
        jcescc=0
    endif
!
!     CREATION DE LA CONNECTIVITE INVERSE
    call cncinv(nommai, zi(jmai), nbma, 'V', cnxinv)
!
    call jeveuo(nommai//'.DIME', 'L', jdime)
    nbmato = zi(jdime+2)
    call wkvect(vecsau, 'V V R', 6*nbmato, jvect)
    do 80 i = 1, 6*nbmato
        zr(jvect+i-1) = 0.d0
80  end do
!
    call jelira(cnxinv, 'NUTIOC', nbno)
!
    call jeveuo(jexnum(cnxinv, 1), 'L', jconi1)
    call jeveuo(jexatr(cnxinv, 'LONCUM'), 'L', jconi2)
!
    call jeveuo(jexnum(nommai//'.CONNEX', 1), 'L', jconx1)
    call jeveuo(jexatr(nommai//'.CONNEX', 'LONCUM'), 'L', jconx2)
    call jeveuo(nommai//'.COORDO    .VALE', 'L', jcoord)
!
    adcar1(1) = jcesd
    adcar1(2) = jcesl
    adcar1(3) = jcesv
    adcar2(1) = jcesdc
    adcar2(2) = jceslc
    adcar2(3) = jcesvc
    lprobm = .false.
!
!     BOUCLE SUR TOUS LES NOEUDS DU MAILLAGE
    maxdif = 0.d0
    do 10 ino = 1, nbno
        nbma = zi(jconi2+ino)-zi(jconi2+ino-1)
        posit = zi(jconi2+ino-1)
!
!       GRACE A LA CONNECTIVITE INVERSE, ON TROUVE
!       LES MAILLES LIEES
        do 20 ima = 1, nbma
            posma = zi(jconi1+posit+ima-2)
!
!         LE COMPORTEMENT DE CNCINV N'EST PAS LE MEME SUIVANT
!         QU'ON DONNE OU NON MESMAI
            if (llimai) then
                numma = zi(jmai+posma-1)
            else
                numma = posma
            endif
            if (numma .eq. 0) goto 20
!
            if ((zr(jvect+6*(numma-1)).eq.0.d0) .and. (zr(jvect+6*( numma-1)+1).eq.0.d0)&
                .and. (zr(jvect+6*(numma-1)+2) .eq.0.d0) .and.&
                (zr(jvect+6*(numma-1)+3).eq.0.d0) .and. (zr(jvect+6*(numma-1)+4).eq.0.d0)&
                .and. (zr(jvect+6*(numma- 1)+5).eq.0.d0)) then
!
                call cccmcr(jcesdd, numma, jrepe, jconx2, jconx1,&
                            jcoord, adcar1, adcar2, ialpha, ibeta,&
                            iepais, jalpha, jbeta, jgamma, ligrmo,&
                            ino, pgl, modeli, ier)
                if (ier .eq. 3) goto 20
                if (ier .eq. 1 .or. ier .eq. 2) then
                    call jenuno(jexnum(nommai//'.NOMMAI', numma), nomail)
                    if (ier .eq. 1) then
                        call utmess('A', 'MODELISA10_5', sk=nomail)
                    else
                        call utmess('A', 'ELEMENTS4_80', sk=nomail)
                    endif
                endif
!
                vl(1) = 1.d0
                vl(2) = 0.d0
                vl(3) = 0.d0
                call utpvlg(1, 3, pgl, vl, vg1)
                vl(1) = 0.d0
                vl(2) = 1.d0
                vl(3) = 0.d0
                call utpvlg(1, 3, pgl, vl, vg2)
!
!           SAUVEGARDE DE LA VALEUR TROUVEE SAUF POUR LES COQUES 3D
                if (modeli .ne. 'COQUE_3D') then
                    do 40 idir = 1, 3
                        zr(jvect+6*(numma-1)+idir-1) = vg1(idir)
40                  continue
                    do 45 idir = 1, 3
                        zr(jvect+3+6*(numma-1)+idir-1) = vg2(idir)
45                  continue
                endif
!
            else
                do 60 idir = 1, 3
                    vg1(idir) = zr(jvect+6*(numma-1)+idir-1)
60              continue
                do 65 idir = 1, 3
                    vg2(idir) = zr(jvect+3+6*(numma-1)+idir-1)
65              continue
            endif
!
!         ON COMPARE LES REPERES DES AUTRES MAILLES
!         LIEES AU NOEUD INO
            do 30 ima2 = ima+1, nbma
                posma = zi(jconi1+posit+ima2-2)
!
                if (llimai) then
                    numma2 = zi(jmai+posma-1)
                else
                    numma2 = posma
                endif
                if (numma2 .eq. 0) goto 30
!
                if ((zr(jvect+6*(numma2-1)).eq.0.d0) .and.&
                    (zr(jvect+ 6*(numma2-1)+1).eq.0.d0) .and.&
                    (zr(jvect+6*(numma2-1)+ 2).eq.0.d0) .and.&
                    (zr(jvect+6*(numma2-1)+3).eq.0.d0) .and.&
                    (zr(jvect+6*(numma2-1)+4).eq.0.d0) .and.&
                    (zr( jvect+6*(numma2-1)+5).eq.0.d0)) then
!
                    call cccmcr(jcesdd, numma2, jrepe, jconx2, jconx1,&
                                jcoord, adcar1, adcar2, ialpha, ibeta,&
                                iepais, jalpha, jbeta, jgamma, ligrmo,&
                                ino, pgl, modeli, ier)
                    if (ier .eq. 3) goto 30
                    if (ier .eq. 1 .or. ier .eq. 2) then
                        call jenuno(jexnum(nommai//'.NOMMAI', numma2), nomail)
                        if (ier .eq. 1) then
                            call utmess('A', 'MODELISA10_5', sk=nomail)
                        else
                            call utmess('A', 'ELEMENTS4_80', sk=nomail)
                        endif
                    endif
!
                    vl(1) = 1.d0
                    vl(2) = 0.d0
                    vl(3) = 0.d0
                    call utpvlg(1, 3, pgl, vl, vg3)
                    vl(1) = 0.d0
                    vl(2) = 1.d0
                    vl(3) = 0.d0
                    call utpvlg(1, 3, pgl, vl, vg4)
!
!             SAUVEGARDE DE LA VALEUR TROUVEE SAUF POUR LES COQUES3D
                    if (modeli .ne. 'COQUE_3D') then
                        do 50 idir = 1, 3
                            zr(jvect+6*(numma2-1)+idir-1) = vg3(idir)
50                      continue
                        do 55 idir = 1, 3
                            zr(jvect+3+6*(numma2-1)+idir-1) = vg4( idir)
55                      continue
                    endif
!
                else
                    do 70 idir = 1, 3
                        vg3(idir) = zr(jvect+6*(numma2-1)+idir-1)
70                  continue
                    do 75 idir = 1, 3
                        vg4(idir) = zr(jvect+3+6*(numma2-1)+idir-1)
75                  continue
                endif
!
                call angvec(vg1, vg3, angle1)
                call angvec(vg2, vg4, angle2)
                if (angle1 .gt. maxtol .or. angle2 .gt. maxtol) then
                    maxdif = max(angle1,maxdif)
                    maxdif = max(angle2,maxdif)
                    lprobm = .true.
                endif
!
30          continue
20      continue
10  end do
!
    if (lprobm) then
        if (cmperr .ne. ' ') then
            tabres(1) = 180.d0*maxdif/pi
            call utmess(cmperr, 'UTILITAI_4', sr=tabres(1))
        endif
        codret = 1
    endif
!
    call jedetr(cnxinv)
    call jedetr(vecsau)
    call detrsd('CHAM_ELEM_S', carsd)
    call detrsd('CHAM_ELEM_S', carcc)
!
    call jedema()
!
end subroutine
