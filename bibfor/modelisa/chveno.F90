subroutine chveno(fonree, noma, nomo)
    implicit none
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterc/r8prem.h"
#include "asterfort/chbord.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/orilma.h"
#include "asterfort/ornorm.h"
#include "asterfort/utmamo.h"
#include "asterfort/utmess.h"
#include "asterfort/utmotp.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=4) :: fonree
    character(len=*) :: noma, nomo
! ======================================================================
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
!
!      OPERATEURS :     AFFE_CHAR_MECA ET AFFE_CHAR_MECA_C
!                                      ET AFFE_CHAR_MECA_F
!                       DEFI_CONTACT
!
!     VERIFICATION DES NORMALES AUX MAILLES SURFACIQUES EN 3D
!     ET LINEIQUES EN 2D
!     V1 : ON VERIFIE QUE LES NORMALES SONT HOMOGENES
!     V2 : ON VERIFIE QUE LES NORMALES SONT SORTANTES
!
!-----------------------------------------------------------------------
    integer :: nbt
    parameter    (nbt = 5 )
    integer :: ier, iret, zero
    integer :: imfac, nbmfac, n, ndim, ndim1, vali
    integer :: iocc, nocc, ic, nbmc, iobj, nbobj, ima, impb, nbmail
    integer :: numail, numa, idtyma, nutyma, nbmapr, nbmabo, ntrait
    integer :: jcoor, jtyma,  jgro,  jmab, jpri, jbor
    integer :: if1, if2, if3, imf1, imf2, ipres, idnor, idtan
    integer :: norien, norie1, norie2, jlima, nbmamo
    real(kind=8) :: dnor
    logical(kind=1) :: reorie, mcfl(nbt)
    character(len=8) :: mot, nomma, nommo, typel
    character(len=16) :: mcft(nbt), motfac, valmc(4), typmc(4)
    character(len=19) :: limamo
    character(len=24) :: grmama, mailma, nogr, nomail
    character(len=24) :: valk(2)
    integer :: iarg
    integer, pointer :: nume_maille(:) => null()
    character(len=24), pointer :: objet(:) => null()
!
    data mcft / 'FACE_IMPO'  , 'PRES_REP' , 'FORCE_COQUE'  ,&
     &            'EFFE_FOND'  , 'ZONE'  /
!
!     LA NORMALE DOIT ETRE SORTANTE:
    data mcfl / .true.       , .true.     , .false.        ,&
     &            .true.       , .true.     /
!     ------------------------------------------------------------------
!
!     INITIALISATIONS
    ier = 0
    zero = 0
    reorie = .false.
!
!     NOMBRE DE MOTS-CLES FACTEUR A VERIFIER
    nbmfac = nbt
!
    nomma = noma
    nommo = nomo
    grmama = nomma//'.GROUPEMA'
    mailma = nomma//'.NOMMAI'
    limamo = '&&CHVENO.MAIL_MODEL'
!
    call getvtx(' ', 'VERI_NORM', scal=mot, nbret=n)
    if (mot .eq. 'NON') nbmfac = 0
!
    ndim = 0
    call dismoi('DIM_GEOM', nomo, 'MODELE', repi=ndim)
!
    call jeexin(nomma//'.TYPMAIL        ', iret)
    if (iret .ne. 0) then
        call jeveuo(nomma//'.TYPMAIL        ', 'L', jtyma)
    endif
    call jeveuo(nomma//'.COORDO    .VALE', 'L', jcoor)
!
    do imfac = 1, nbmfac
        motfac = mcft(imfac)
!
!       CAS OU UN MOT CLE N'EXISTE QUE POUR CERTAINS CATALOGUES
!       (PAR EXEMPLE EFFE_FOND)
        if (getexm(motfac,' ') .eq. 0) goto 100
!
        call getfac(motfac, nocc)
        do iocc = 1, nocc
!         POUR CERTAINS MOTS-CLES, IL NE FAUT TESTER QUE
!         POUR CERTAINS CHARGEMENTS
            if (motfac .eq. 'FACE_IMPO') then
                ipres = utmotp(fonree,motfac,iocc,'PRES')
                idnor = utmotp(fonree,motfac,iocc,'DNOR')
                idtan = utmotp(fonree,motfac,iocc,'DTAN')
                if (ipres .eq. 0 .and. idnor .eq. 0 .and. idtan .eq. 0) goto 200
                if (idnor .ne. 0) then
                    if (fonree .eq. 'REEL') then
                        call getvr8(motfac, 'DNOR', iocc=iocc, scal=dnor, nbret=n)
                        if (abs(dnor) .le. r8prem()) goto 200
                    endif
                endif
            else if (motfac.eq.'FORCE_COQUE') then
                ipres = utmotp(fonree,motfac,iocc,'PRES')
                if1 = utmotp(fonree,motfac,iocc,'F1  ')
                if2 = utmotp(fonree,motfac,iocc,'F2  ')
                if3 = utmotp(fonree,motfac,iocc,'F3  ')
                imf1 = utmotp(fonree,motfac,iocc,'MF1 ')
                imf2 = utmotp(fonree,motfac,iocc,'MF2 ')
                if (ipres .eq. 0 .and. if1 .eq. 0 .and. if2 .eq. 0 .and. if3 .eq. 0 .and. imf1&
                    .eq. 0 .and. imf2 .eq. 0) goto 200
            endif
!
            if (motfac .eq. 'ZONE') then
                nbmc = 4
                valmc(1) = 'GROUP_MA_ESCL'
                valmc(2) = 'GROUP_MA_MAIT'
                valmc(3) = 'MAILLE_ESCL'
                valmc(4) = 'MAILLE_MAIT'
                typmc(1) = 'GROUP_MA'
                typmc(2) = 'GROUP_MA'
                typmc(3) = 'MAILLE'
                typmc(4) = 'MAILLE'
            else
                nbmc = 2
                valmc(1) = 'GROUP_MA'
                valmc(2) = 'MAILLE'
                typmc(1) = 'GROUP_MA'
                typmc(2) = 'MAILLE'
            endif
!
! ---     RECUPERATION DE LA DIMENSION DU PROBLEME
!
            do ic = 1, nbmc
                call getvtx(motfac, valmc(ic), iocc=iocc, nbval=0, nbret=nbobj)
                if (nbobj .eq. 0) goto 210
!
                nbobj = -nbobj
                AS_ALLOCATE(vk24=objet, size=nbobj)
                call getvem(noma, typmc(ic), motfac, valmc(ic), iocc,&
                            iarg, nbobj, objet, nbobj)
                if (typmc(ic) .eq. 'GROUP_MA') then
                    do iobj = 1, nbobj
                        nogr = objet(iobj)
                        if (motfac .eq. 'ZONE') then
!
! ---             RECUPERATION DU NOMBRE DE MAILLES DU GROUP_MA :
!                 ---------------------------------------------
                            call jelira(jexnom(grmama, nogr), 'LONUTI', nbmail)
                            call jeveuo(jexnom(grmama, nogr), 'L', jgro)
!
                            do ima = 1, nbmail
                                numail = zi(jgro-1+ima)
                                call jenuno(jexnum(mailma, numail), nomail)
!
! ---               NUMERO DE LA MAILLE
!                   ------------------
                                call jenonu(jexnom(nomma//'.NOMMAI', nomail), numa)
                                call jeveuo(nomma//'.TYPMAIL', 'L', idtyma)
                                nutyma = zi(idtyma+numa-1)
!
! ---               TYPE DE LA MAILLE :
!                   -----------------
                                call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), typel)
!
! ---               CAS D'UNE MAILLE POINT
!                   ----------------------
                                if (typel(1:3) .eq. 'POI') then
!                     ON SAUTE
                                    goto 211
!
! ---               CAS D'UNE MAILLE SEG
!                   --------------------
                                else if (typel(1:3) .eq. 'SEG') then
                                    ndim1 = 2
                                    if (ndim .ne. ndim1) then
!                       ON SAUTE
                                        goto 211
                                    endif
!
                                endif
                            end do
!
! ---           FIN DE BOUCLE SUR LES MAILLES DU GROUP_MA
!
                        endif
                        norie1 = 0
                        norie2 = 0
                        call jelira(jexnom(grmama, nogr), 'LONUTI', nbmail)
                        call jeveuo(jexnom(grmama, nogr), 'L', jgro)
!
                        if (mcfl(ic) .and. (nbmail.gt.0)) then
!
                            call wkvect('&&CHVENO.MAILLE_BORD', 'V V I', nbmail, jmab)
                            call chbord(nomo, nbmail, zi(jgro), zi( jmab), nbmapr,&
                                        nbmabo)
                            if (nbmapr .eq. nbmail .and. nbmabo .eq. 0) then
                                call ornorm(nomma, zi(jgro), nbmail, reorie, norie1)
                                elseif ( (nbmapr.eq.0 .and.&
                            nbmabo.eq.nbmail) .or. (motfac .eq.&
                            'ZONE') ) then
                                if (motfac .eq. 'ZONE') then
                                    nbmamo = 0
                                    jlima = 1
                                else
                                    call utmamo(nommo, nbmamo, limamo)
                                    call jeveuo(limamo, 'L', jlima)
                                endif
                                call orilma(nomma, ndim, zi(jgro), nbmail, norie1,&
                                            ntrait, reorie, nbmamo, zi(jlima ))
                                call jedetr(limamo)
                                elseif ( nbmapr.eq.0 .and. nbmabo.eq.0 )&
                            then
                                call ornorm(nomma, zi(jgro), nbmail, reorie, norie1)
                            else
                                call wkvect('&&CHVENO.PRIN', 'V V I', nbmapr, jpri)
                                call wkvect('&&CHVENO.BORD', 'V V I', nbmabo, jbor)
                                nbmapr = 0
                                nbmabo = 0
                                do impb = 1, nbmail
                                    if (zi(jmab+impb-1) .eq. 0) then
                                        nbmapr = nbmapr + 1
                                        zi(jpri+nbmapr-1) = zi(jgro+ impb-1)
                                    else
                                        nbmabo = nbmabo + 1
                                        zi(jbor+nbmabo-1) = zi(jgro+ impb-1)
                                    endif
                                end do
                                call ornorm(nomma, zi(jpri), nbmapr, reorie, norie1)
                                call orilma(nomma, ndim, zi(jbor), nbmabo, norie1,&
                                            ntrait, reorie, 0, [0])
                                call jedetr('&&CHVENO.PRIN')
                                call jedetr('&&CHVENO.BORD')
                            endif
                            call jedetr('&&CHVENO.MAILLE_BORD')
                        else
                            call ornorm(nomma, zi(jgro), nbmail, reorie, norie2)
                        endif
                        norien = norie1 + norie2
                        if (norien .ne. 0) then
                            ier = ier + 1
                            valk(1) = nogr
                            vali = norien
                            call utmess('E', 'MODELISA8_56', sk=valk(1), si=vali)
                        endif
                    end do
!
! ----------CAS DES MAILLES :
!           ---------------
                else
                    AS_ALLOCATE(vi=nume_maille, size=nbobj)
                    do iobj = 1, nbobj
                        nomail = objet(iobj)
                        call jenonu(jexnom(nomma//'.NOMMAI', nomail), numa)
                        nume_maille(iobj) = numa
                        if (motfac .eq. 'ZONE') then
                            call jeveuo(nomma//'.TYPMAIL', 'L', idtyma)
                            nutyma = zi(idtyma+numa-1)
                            call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), typel)
!
! ---             CAS D'UNE MAILLE POINT
!                 ----------------------
                            if (typel(1:3) .eq. 'POI') then
!                   ON SAUTE
                                goto 211
!
! ---             CAS D'UNE MAILLE SEG
!                 --------------------
                            else if (typel(1:3) .eq. 'SEG') then
                                ndim1 = 2
                                if (ndim .ne. ndim1) then
!                     ON SAUTE
                                    goto 211
                                endif
                            endif
!
                        endif
                    end do
                    norie1 = 0
                    norie2 = 0
                    if (mcfl(ic)) then
                        call wkvect('&&CHVENO.MAILLE_BORD', 'V V I', nbobj, jmab)
                        call chbord(nomo, nbobj, nume_maille, zi(jmab), nbmapr,&
                                    nbmabo)
                        if (nbmapr .eq. nbobj .and. nbmabo .eq. 0) then
                            call ornorm(nomma, nume_maille, nbobj, reorie, norie1)
                            elseif ( (nbmapr.eq.0 .and. nbmabo.eq.nbobj)&
                        .or. (motfac .eq. 'ZONE') ) then
                            if (motfac .eq. 'ZONE') then
                                nbmamo = 0
                                jlima = 1
                            else
                                call utmamo(nommo, nbmamo, limamo)
                                call jeveuo(limamo, 'L', jlima)
                            endif
                            call orilma(nomma, ndim, nume_maille, nbobj, norie1,&
                                        ntrait, reorie, nbmamo, zi( jlima))
                            call jedetr(limamo)
                        else if (nbmapr.eq.0 .and. nbmabo.eq.0) then
                            call ornorm(nomma, nume_maille, nbobj, reorie, norie1)
                        else
                            call wkvect('&&CHVENO.PRIN', 'V V I', nbmapr, jpri)
                            call wkvect('&&CHVENO.BORD', 'V V I', nbmabo, jbor)
                            nbmapr = 0
                            nbmabo = 0
                            do impb = 1, nbobj
                                if (zi(jmab+impb-1) .eq. 0) then
                                    nbmapr = nbmapr + 1
                                    zi(jpri+nbmapr-1) = nume_maille(impb)
                                else
                                    nbmabo = nbmabo + 1
                                    zi(jbor+nbmabo-1) = nume_maille(impb)
                                endif
                            end do
                            call ornorm(nomma, zi(jpri), nbmapr, reorie, norie1)
                            call orilma(nomma, ndim, zi(jbor), nbmabo, norie1,&
                                        ntrait, reorie, 0, [0])
                            call jedetr('&&CHVENO.PRIN')
                            call jedetr('&&CHVENO.BORD')
                        endif
                        call jedetr('&&CHVENO.MAILLE_BORD')
                    else
                        call ornorm(nomma, nume_maille, nbobj, reorie, norie2)
                    endif
                    norien = norie1 + norie2
                    if (norien .ne. 0) then
                        ier = ier + 1
                        valk(1) = nomail
                        call utmess('E', 'MODELISA8_57', sk=valk(1))
                    endif
                endif
211             continue
                AS_DEALLOCATE(vi=nume_maille)
                AS_DEALLOCATE(vk24=objet)
210             continue
            end do
200         continue
        end do
100     continue
    end do
!
    if (ier .ne. 0) then
        call utmess('F', 'MODELISA4_24')
    endif
!
end subroutine
