subroutine rslesd(result, nuord, modele, materi, carele,&
                  excit, iexcit)
    implicit none
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nuord, iexcit
    character(len=8) :: result, modele, carele, materi
    character(len=19) :: excit
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
!
!     BUT:
!         LIRE OU ECRIRE DES NOMS DE CONCEPT DE LA SD RESULTAT ET
!         D'EXPLOITER DES OBJETS DE LA SD CORRESPONDANT AUX CHARGES.
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   RESULT : NOM DE LA SD RESULTAT
! IN   NUORD  : NUMERO D'ORDRE
!
!      SORTIE :
!-------------
! OUT  MODELE : NOM DU MODELE
! OUT  MATERI : NOM DU CHAMP MATERIAU
! OUT  CARELE : NOM DE LA CARACTERISTIQUE ELEMENTAIRE CARA_ELEM
! OUT  EXCIT  : NOM DE LA SD INFO_CHARGE
! OUT  IEXCIT : INDICE DEFINISSANT L'ORIGINE DU CHARGEMENT
!                      UTILISE LORS DES CALCLULS
!                      0 : LE CHARGEMENT EST ISSU DE LA SD RESULTAT
!                      1 : LE CHARGEMENT EST FOURNI PAR L'UTILISATEUR
!
! ......................................................................
!
!
    integer :: jpara, n1, n2, n3, n4, iex, jlcha, jinfc, jfcha, ncha
    integer :: ilu, isd, nchalu, nchasd, lchalu, fchalu, vali(2)
!
    character(len=6) :: nompro
    character(len=8) :: nomsd, nomlu, fonclu, k8b, foncsd
    character(len=16) :: type, nomcmd
    character(len=19) :: kcha, kfon
    character(len=24) :: excisd, valk(4)
!
    parameter(nompro='RSLESD')
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!--- INITIALISATIONS
!               123456789012345678901234
    kcha = '&&'//nompro//'.CHARGE    '
    kfon = '&&'//nompro//'.FONC_MULT '
    iexcit = 0
    n4 = 0
!
    call getres(k8b, type, nomcmd)
!
!
!==========================================================
!
!     T R A I T E M E N T  DU  M O D E L E
!
!==========================================================
!
!
!---  RECUPERATION DU NOM DU MODELE
!
    n1=0
    nomlu=' '
    if (getexm(' ','MODELE') .eq. 1) then
        call getvid(' ', 'MODELE', scal=nomlu, nbret=n1)
    endif
!
    call rsadpa(result, 'L', 1, 'MODELE', nuord,&
                0, sjv=jpara, styp=k8b)
    nomsd=zk8(jpara)
!
    if (nomsd .ne. ' ') then
        if (n1 .eq. 0) then
            modele = nomsd
        else if (nomsd.eq.nomlu) then
            modele = nomlu
        else
            call utmess('A', 'UTILITAI4_37')
            modele = nomlu
        endif
    else
        if (n1 .ne. 0) then
            modele = nomlu
        else
            modele = ' '
        endif
    endif
!
    if (nomsd .eq. ' ' .and. nomlu .ne. ' ') then
        call rsadpa(result, 'E', 1, 'MODELE', nuord,&
                    0, sjv=jpara, styp=k8b)
        zk8(jpara)=modele
    endif
!
!==========================================================
!
!     T R A I T E M E N T   D U   C A R A _ E L E M
!
!==========================================================
!
!--- RECUPERATION DU NOM DU CARA_ELEM
!
    call rsadpa(result, 'L', 1, 'CARAELEM', nuord,&
                0, sjv=jpara, styp=k8b)
    nomsd=zk8(jpara)
    if (getexm(' ','CARA_ELEM') .eq. 1) then
        call getvid(' ', 'CARA_ELEM', scal=nomlu, nbret=n2)
    else
        n2=0
        nomlu=' '
    endif
!
    if (nomsd .ne. ' ') then
        if (n2 .eq. 0) then
            carele = nomsd
        else if (nomsd.eq.nomlu) then
            carele = nomlu
        else
            call utmess('A', 'UTILITAI4_38')
            carele = nomlu
        endif
    else
        if (n2 .ne. 0) then
            carele = nomlu
        else
            carele = ' '
        endif
    endif
!
    if (nomsd .eq. ' ' .and. nomlu .ne. ' ') then
        call rsadpa(result, 'E', 1, 'CARAELEM', nuord,&
                    0, sjv=jpara, styp=k8b)
        zk8(jpara)=carele
    endif
!
!==========================================================
!
!     T R A I T E M E N T   D U   M A T E R I A U
!
!==========================================================
!
!
!---  RECUPERATION DU NOM DU CHAMP MATERIAU
!
    if (getexm(' ','CHAM_MATER') .eq. 1) then
        call getvid(' ', 'CHAM_MATER', scal=nomlu, nbret=n3)
    else
        n3=0
        nomlu=' '
    endif
!
    call rsadpa(result, 'L', 1, 'CHAMPMAT', nuord,&
                0, sjv=jpara, styp=k8b)
    nomsd=zk8(jpara)
!
    if (nomsd .ne. ' ') then
        if (n3 .eq. 0) then
            materi = nomsd
        else if (nomsd.eq.nomlu) then
            materi = nomlu
        else
            call utmess('A', 'UTILITAI4_39')
            materi = nomlu
        endif
    else
        if (n3 .ne. 0) then
            materi = nomlu
        else
            materi = ' '
        endif
    endif
!
    if (nomsd .eq. ' ' .and. nomlu .ne. ' ') then
        call rsadpa(result, 'E', 1, 'CHAMPMAT', nuord,&
                    0, sjv=jpara, styp=k8b)
        zk8(jpara)=materi
    endif
!
!
!==========================================================
!
!     T R A I T E M E N T   D E S    C H A R G E M E N T S
!
!==========================================================
!
!--- RECUPERATION DES CHARGEMENTS 'EXCIT'
!
!--- LECTURE DES INFORMATIONS UTILISATEUR
!
    nchalu=0
!
    if (getexm('EXCIT','CHARGE') .eq. 1) then
        call getfac('EXCIT', nchalu)
!
        if (nchalu .ne. 0) then
            call wkvect(kcha, 'V V K8', nchalu, lchalu)
            call wkvect(kfon, 'V V K8', nchalu, fchalu)
!
            do 10 iex = 1, nchalu
                call getvid('EXCIT', 'CHARGE', iocc=iex, scal=zk8(lchalu+ iex-1), nbret=n1)
!
                call getvid('EXCIT', 'FONC_MULT', iocc=iex, scal=fonclu, nbret=n2)
                if (n2 .ne. 0) then
                    zk8(fchalu+iex-1) = fonclu
                endif
10          continue
        endif
    endif
!
    if (getexm(' ','CHARGE') .eq. 1) then
        call getvid(' ', 'CHARGE', nbval=0, nbret=n4)
        ncha = -n4
        nchalu = max(1,ncha)
        call wkvect(kcha, 'V V K8', nchalu, lchalu)
        call getvid(' ', 'CHARGE', nbval=ncha, vect=zk8(lchalu), nbret=n4)
    endif
!
!--- LECTURE DES INFORMATIONS CONTENUES DANS LA SD RESULTAT
!
    call rsadpa(result, 'L', 1, 'EXCIT', nuord,&
                0, sjv=jpara, styp=k8b)
    excisd=zk24(jpara)
!
!--- VERIFICATIONS ET AFFECTATIONS
!
!     IEXCIT = 0 SD RESULTAT
!            = 1 UTILISATEUR
    if (nomcmd .eq. 'POST_ELEM') then
        if (n4 .eq. 0) then
            iexcit = 0
            nchalu = 0
        else
            iexcit = 1
        endif
    else
        if (nchalu .ne. 0) iexcit = 1
    endif
!
    if (nchalu .eq. 0 .and. excisd(1:1) .eq. ' ') iexcit = 1
!
    if (excisd .ne. ' ') then
        excit = excisd(1:19)
        call jeveuo(excit(1:19)//'.LCHA', 'L', jlcha)
        call jeveuo(excit(1:19)//'.INFC', 'L', jinfc)
        call jeveuo(excit(1:19)//'.FCHA', 'L', jfcha)
        nchasd = zi(jinfc)
    endif
!
!--- VERIFICATIONS DES CHARGEMENTS
!
    if ((nchalu.ne.0) .and. (excisd.ne.' ')) then
!
!--- VERIFICATION DE LA COHERENCE DU NOMBRE DE CHARGES ENTRE
!    CELLES PRESENTES DANS LA SD RESULTAT ET CELLES FOURNIES
!    PAR L'UTILISATEUR
        if (nchalu .ne. nchasd) then
            vali(1)=nchalu
            vali(2)=nchasd
            call utmess('A', 'CALCULEL6_65', ni=2, vali=vali)
        endif
!
!--- VERIFICATIONS DU NOM DES CHARGEMENTS
!
        do 40 ilu = 1, nchalu
            do 20 isd = 1, nchasd
                if (zk8(lchalu-1+ilu) .eq. zk24(jlcha-1+isd)(1:8)) goto 30
20          continue
            call utmess('A', 'UTILITAI4_40')
30          continue
40      continue
!
!--- VERIFICATIONS DU NOM DES FONCTION MULTIPLICATRICES
!
        if (nomcmd .ne. 'POST_ELEM') then
            do 70 ilu = 1, nchalu
                do 50 isd = 1, nchasd
                    foncsd = zk24(jfcha-1+isd)(1:8)
                    if (foncsd(1:2) .eq. '&&') foncsd = ' '
                    if (zk8(fchalu-1+ilu) .eq. foncsd) goto 60
50              continue
                call utmess('A', 'UTILITAI4_41')
60              continue
70          continue
        endif
!
!--- VERIFICATIONS DES COUPLES NOM DE CHARGE ET FONCTION MULTIPLICATRICE
!    FOURNI PAR L'UTILISATEUR AVEC CEUX PRESENTS DANS LA SD RESULTAT
!
        if (nomcmd .ne. 'POST_ELEM') then
            do 80 ilu = 1, nchalu
                do 90 isd = 1, nchasd
                    if (zk8(lchalu-1+ilu) .eq. zk24(jlcha-1+isd)(1:8)) then
                        foncsd = zk24(jfcha-1+isd)(1:8)
                        if (foncsd(1:2) .eq. '&&') foncsd = ' '
                        if (zk8(fchalu-1+ilu) .eq. foncsd) goto 95
                        valk(1)=zk8(lchalu-1+ilu)
                        valk(2)=zk8(fchalu-1+ilu)
                        valk(3)=zk24(jlcha-1+isd)(1:8)
                        valk(4)=foncsd
                        call utmess('A', 'CALCULEL6_66', nk=4, valk=valk)
                    endif
90              continue
95              continue
80          continue
        endif
    endif
!
!--- MENAGE
!
    call jedetr(kcha)
    call jedetr(kfon)
!
    call jedema()
end subroutine
