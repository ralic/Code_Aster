subroutine medome(modele, mate, cara, kcha, ncha,&
                  ctyp, result)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/rslesd.h"
#include "asterfort/rsutnu.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: ncha
    character(len=4) :: ctyp
    character(len=8) :: modele, cara, result
    character(len=24) :: mate
    character(len=*) :: kcha
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     SAISIE ET VERIFICATION DE LA COHERENCE DES DONNEES MECANIQUES
!     DU PROBLEME
! ----------------------------------------------------------------------
! OUT : MODELE : NOM DU MODELE
! OUT : MATE   : CHAMP MATERIAU
! OUT : CARA   : NOM DU CHAMP DE CARACTERISTIQUES
! IN  : KCHA   : NOM JEVEUX POUR STOCKER LES CHARGES
! OUT : NCHA   : NOMBRE DE CHARGES
! OUT : CTYP   : TYPE DE CHARGE
! IN  : RESULT : NOM DE LA SD RESULTAT
!
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: iexcit, iret, icha
    character(len=6) :: nompro
!-----------------------------------------------------------------------
    integer :: i, ie, ier, ierd, in, inuord
    integer :: iordr, jordr, n1, n2, n3
    integer :: n4, nbordr, nc, np, nuord
    real(kind=8) :: prec
!-----------------------------------------------------------------------
    parameter (nompro='MEDOME')
    character(len=8) :: k8b, nomo, materi, blan8, modnew
    character(len=16) :: concep, nomcmd, phenom
    character(len=19) :: excit, knum
    character(len=8) :: crit
    aster_logical :: lpost
    character(len=24), pointer :: lcha(:) => null()
    integer, pointer :: infc(:) => null()
    call jemarq()
!
!              12345678
    blan8 = '        '
    ier = 0
    ctyp = ' '
    modele = ' '
    nomo = ' '
    cara = ' '
    materi = ' '
    iexcit = 1
    lpost = .false.
    n1 = 0
!
    call getres(k8b, concep, nomcmd)
!
    lpost = (nomcmd.eq.'POST_ELEM')
!
    if (lpost .and. (result(1:1).ne.' ')) then
        call getvis(' ', 'NUME_ORDRE', scal=nuord, nbret=inuord)
        if (inuord .eq. 0 .and. nomcmd .eq. 'POST_ELEM') then
!
!     L'UTILISATEUR N'A PAS FOURNI DE NUMERO D'ORDRE :
!     RECUPERATION DU PREMIER NUMERO D'ORDRE DANS LA SD RESULTAT
!     ----------------------------------------------------------
            knum = '&&'//nompro//'.NUME_ORDRE'
            call getvr8(' ', 'PRECISION', scal=prec, nbret=np)
            call getvtx(' ', 'CRITERE', scal=crit, nbret=nc)
            call rsutnu(result, ' ', 0, knum, nbordr,&
                        prec, crit, iret)
            call jeveuo(knum, 'L', jordr)
            nuord = zi(jordr)
!
!     RECUPERATION DU MODELE, MATERIAU, CARA_ELEM et EXCIT
!     POUR LE NUMERO d'ORDRE NUORD
!
            call rslesd(result, nuord, modele, materi, cara,&
                        excit, iexcit)
!
!  VERIFICATION DE L'UNICITE DU MODELE DANS LE RESULTAT
!  SINON ON ARRETE EN ERREUR FATALE
!
            do iordr = 2, nbordr
                nuord = zi(jordr+iordr-1)
                call rslesd(result, nuord, modnew, materi, cara,&
                            excit, iexcit)
                if (modnew .ne. modele) then
                    call utmess('F', 'CALCULEL7_3')
                endif
            end do
        else
            call rslesd(result, nuord, modele, materi, cara,&
                        excit, iexcit)
        endif
!
        if (materi .ne. blan8) then
            call rcmfmc(materi, mate)
        else
            mate = ' '
        endif
!
        if (iexcit .eq. 0 .and. nomcmd .eq. 'POST_ELEM') then
            kcha=excit(1:19)
        endif
    else
!
        call getvid(' ', 'MODELE', scal=modele, nbret=n1)
        if (n1 .eq. 0) then
            call utmess('F', 'CALCULEL6_84')
        endif
!
        call getvid(' ', 'CARA_ELEM', scal=cara, nbret=n2)
        call dismoi('EXI_RDM', modele, 'MODELE', repk=k8b)
        if ((n2.eq.0) .and. (k8b(1:3).eq.'OUI')) then
            call utmess('A', 'CALCULEL3_39')
        endif
!
!
        call getvid(' ', 'CHAM_MATER', scal=materi, nbret=n3)
        call dismoi('BESOIN_MATER', modele, 'MODELE', repk=k8b)
        if ((nomcmd.ne.'CALC_MATR_ELEM') .and. (n3.eq.0) .and. (k8b(1:3) .eq.'OUI')) then
            call utmess('A', 'CALCULEL3_40')
        endif
!
        if (n3 .ne. 0) then
            call rcmfmc(materi, mate)
        else
            mate = ' '
        endif
    endif
!
!   TRAITEMENT DU CHARGEMENT
!
!   SI IEXCIT=1 ON PREND LE CHARGEMENT DONNE PAR L'UTILISATEUR
!
    if (iexcit .eq. 1) then
        call getvid(' ', 'CHARGE', nbval=0, nbret=n4)
        ncha = -n4
        call wkvect(kcha, 'V V K8', max(1, ncha), icha)
        call getvid(' ', 'CHARGE', nbval=ncha, vect=zk8(icha), nbret=n4)
!
!     -- ON VERIFIE QUE LES CHARGES PORTENT TOUTES SUR LE MEME MODELE.
        if (ncha .gt. 0) then
            call dismoi('NOM_MODELE', zk8(icha), 'CHARGE', repk=nomo)
            do i = 1, ncha
                call dismoi('NOM_MODELE', zk8(icha-1+i), 'CHARGE', repk=k8b)
                if (k8b .ne. nomo) then
                    ier = ier + 1
                    call utmess('E', 'CALCULEL3_41')
                endif
            end do
!
!        --- ON VERIFIE QUE LES CHARGES PORTENT SUR LE MODELE
!                               EVENTUELEMENT DONNE EN ARGUMENT ---
            if (n1 .ne. 0 .and. modele .ne. nomo) then
                ier = ier + 1
                call utmess('E', 'CALCULEL3_42')
            endif
!
!        --- VERIFICATION DU TYPE DE CHARGEMENT ---
            call dismoi('TYPE_CHARGE', zk8(icha), 'CHARGE', repk=ctyp)
            do i = 1, ncha
                call dismoi('TYPE_CHARGE', zk8(icha-1+i), 'CHARGE', repk=k8b)
                if ((k8b(1:4).ne.'MECA') .and. (k8b(1:4).ne.'CIME') .and.&
                    (k8b(1:4).ne.'THER') .and. (k8b(1:4).ne.'ACOU')) then
                    ier = ier + 1
                    call utmess('E', 'CALCULEL3_43')
                endif
            end do
        endif
    else
!
!   SI IEXCIT=0 ON PREND LE CHARGEMENT DONNE PAR LA SD RESULTAT
!
        call jeveuo(excit//'.INFC', 'L', vi=infc)
        ncha=infc(1)
        call jeveuo(excit//'.LCHA', 'L', vk24=lcha)
        call jedetr(kcha)
        call wkvect(kcha, 'V V K8', ncha, icha)
        call dismoi('PHENOMENE', modele, 'MODELE', repk=phenom, arret='C',&
                    ier=ierd)
        ctyp=phenom(1:4)
        in=0
        do i = 1, ncha
            call jeexin(lcha(i)(1:8)//'.TYPE', ie)
!          ON TESTE SI LA CHARGE EST NON VIDE
            if (ie .ne. 0) then
                call dismoi('TYPE_CHARGE', lcha(i)(1:8), 'CHARGE', repk=k8b)
!          ON STOCKE LES CHARGES DONT LE TYPE CORRESPOND A CTYP
                if (ctyp .eq. k8b(1:4)) then
                    zk8(icha+in)=lcha(i)(1:8)
                    in=in+1
                endif
            endif
        end do
        ncha=in
!
!        ON VERIFIE QUE LES CHARGES RECUPEREES REPOSENT
!        TOUTES SUR LE MEME MODELE
        do i = 1, ncha
            call dismoi('NOM_MODELE', zk8(icha+i-1), 'CHARGE', repk=nomo)
            if (nomo .ne. modele) then
                call utmess('F', 'CALCULEL3_44')
            endif
        end do
    endif
!
    if (ier .ne. 0) then
        call utmess('F', 'CALCULEL3_45')
    endif
!
    call jedema()
end subroutine
