subroutine sfifj(nomres)
    implicit none
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
!-----------------------------------------------------------------------
!     CALCUL DE LA FONCTION ACCEPTANCE
!     TURBULENCE DE COUCHE LIMITE
!     AUTEUR : G. ROUSSEAU
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/accep1.h"
#include "asterfort/accep2.h"
#include "asterfort/accept.h"
#include "asterfort/chpver.h"
#include "asterfort/dspprs.h"
#include "asterfort/evalis.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nfinit, nfin, nbm, nbpoin, nbid
    integer :: npoin, iff,  lvale, ibid, in
    integer :: im1, im2,     nvecx, nvecy
    integer :: nveco, ier, ncham, jpara
    integer :: lnumi, lnumj, lfreq, mxval, nbabs, ij
    real(kind=8) :: fmin, fmax, finit, ffin, df, f, prs
    real(kind=8) :: kste, uflui, dhyd, rho, jc, fcoupu, fmodel
    real(kind=8) :: dir(3, 3), fcoup
    real(kind=8) :: deuxpi, puls, uc, ut, long1, long2
    real(kind=8) :: valr
    character(len=8) :: k8b, nomres, is
    character(len=8) :: spectr, method
    character(len=19) :: base, fonct, chamno, pg, phi, sphi
    character(len=24) :: ligrmo
    character(len=24) :: chnumi, chnumj, chfreq, chvale
    logical :: yang
    real(kind=8), pointer :: vecx(:) => null()
    real(kind=8), pointer :: vecy(:) => null()
    real(kind=8), pointer :: vecz(:) => null()
    integer, pointer :: ordr(:) => null()
    character(len=16), pointer :: vate(:) => null()
    real(kind=8), pointer :: vare(:) => null()
!
    data         deuxpi/6.28318530718d0/,yang/.false./
!
!-----------------------------------------------------------------------
    call jemarq()
!
! RECHERCHE DE LA PRESENCE D'UN CHAMNO
    call getvid(' ', 'CHAM_NO', nbval=0, nbret=ncham)
!
    if (ncham .eq. 0) then
!
! RECUPERATION DE LA BASE MODALE
        call getvid(' ', 'MODE_MECA', scal=base, nbret=ibid)
!
        call jeveuo(base//'.ORDR', 'L', vi=ordr)
        call jelira(base//'.ORDR', 'LONUTI', nbm)
!
! RECUPERATION DE LA FREQUENCE MINIMALE ET MAX DES MODES
!
        call rsadpa(base, 'L', 1, 'FREQ', ordr(1),&
                    0, sjv=jpara, styp=k8b)
        fmin = zr(jpara)
        call rsadpa(base, 'L', 1, 'FREQ', ordr(nbm),&
                    0, sjv=jpara, styp=k8b)
        fmax = zr(jpara)
    else
        call getvid(' ', 'CHAM_NO', scal=chamno, nbret=ibid)
        call chpver('F', chamno, 'NOEU', 'DEPL_R', ier)
        call getvid(' ', 'CHAM_NO', nbval=0, nbret=ncham)
        nbm = -ncham
    endif
!
! RECUPERATION DE LA FREQUENCE MINIMALE ET MAX DE LA PLAGE
! DE FREQUENCE ETUDIEE
!
    call getvr8(' ', 'FREQ_INIT', scal=finit, nbret=nfinit)
    call getvr8(' ', 'FREQ_FIN', scal=ffin, nbret=nfin)
    if ((ffin-finit) .lt. r8prem()) then
        call utmess('F', 'MODELISA6_97')
    endif
!
    if (nfinit .lt. 0) then
        if (ncham .ne. 0) then
            call utmess('F', 'MODELISA6_98')
        endif
        valr = fmin
        call utmess('I', 'MODELISA9_15', sr=valr)
        finit=fmin
    endif
    if (nfin .lt. 0) then
        if (ncham .ne. 0) then
            call utmess('F', 'MODELISA6_99')
        endif
        valr = fmax
        call utmess('I', 'MODELISA9_16', sr=valr)
        ffin=fmax
    endif
!
! DISCRETISATION FREQUENTIELLE
    call getvis(' ', 'NB_POIN', scal=nbpoin, nbret=npoin)
!
! PAS FREQUENTIEL
    df = (ffin-finit) / (nbpoin-1)
    if (df .lt. r8prem()) then
        call utmess('F', 'MODELISA7_1')
    endif
!
! CALCUL DE L'ACCEPTANCE
!
    call getvid(' ', 'SPEC_TURB', scal=spectr, nbret=ibid)
    call jeveuo(spectr//'           .VARE', 'L', vr=vare)
    call jeveuo(spectr//'           .VATE', 'L', vk16=vate)
!
! RECUPERATION DES CONSTANTES DU SPECTRES DU
! MODELE 5 : CONSTANT PUIS NUL POUR FR > 10
!
    if (vate(1) .eq. 'SPEC_CORR_CONV_1') then
        uflui = vare(3)
        rho = vare(4)
        fcoupu = vare(5)
        kste = vare(6)
        dhyd = vare(7)
! LONGUEURS DE CORRELATION
        long1=vare(1)
        long2=vare(2)
! VITESSE CONVECTIVE RADIALE (METHODE AU-YANG)
        uc=vare(8)*uflui
! VITESSE CONVECTIVE ORTHORADIALE (METHODE AU-YANG)
        ut=vare(9)*uflui
!
! CALCUL DE LA FREQUENCE DE COUPURE PRONE PAR LE MODELE
! ET COMPARAISON AVEC LA FREQUENCE DE COUPURE DONNEE PAR
! L UTILISATEUR
!
        fmodel = 10.d0 * uflui / dhyd
        if (fcoupu .le. fmodel) then
            valr = fcoupu
            call utmess('I', 'MODELISA9_17', sr=valr)
            valr = fmodel
            call utmess('I', 'MODELISA9_18', sr=valr)
            call utmess('I', 'MODELISA9_19')
            fcoup = fcoupu * dhyd / uflui
        else
            valr = fcoupu
            call utmess('I', 'MODELISA9_20', sr=valr)
            valr = fmodel
            call utmess('I', 'MODELISA9_21', sr=valr)
            call utmess('I', 'MODELISA9_22')
            fcoup = 10.d0
        endif
!
! RECUPERATION DE LA METHOD DE LA FONCTION
! DE COHERENCE
!
        method = vate(11)(1:8)
    else if (vate(1).eq.'SPEC_CORR_CONV_2') then
        uflui=vare(1)
        fcoup=vare(2)
        method=vate(5)(1:8)
        fonct =vate(2)
    else if (vate(1).eq.'SPEC_CORR_CONV_3') then
        fonct =vate(2)
        goto 10
    endif
!
!
! RECUPERATION DES DIRECTIONS DU PLAN DE LA PLANCHE
    if (method(1:6) .eq. 'CORCOS') then
        call getvr8(' ', 'VECT_X', nbval=0, nbret=nvecx)
        nvecx=-nvecx
        if (nvecx .gt. 0) then
            AS_ALLOCATE(vr=vecx, size=3)
            call getvr8(' ', 'VECT_X', nbval=nvecx, vect=vecx, nbret=nbid)
        endif
        call getvr8(' ', 'VECT_Y', nbval=0, nbret=nvecy)
        nvecy=-nvecy
        if (nvecy .gt. 0) then
            AS_ALLOCATE(vr=vecy, size=3)
            call getvr8(' ', 'VECT_Y', nbval=nvecy, vect=vecy, nbret=nbid)
        endif
        if (nvecx .lt. 0 .or. nvecy .lt. 0) then
            call utmess('F', 'MODELISA7_2')
        endif
!
! VECTEUR Z LOCAL = VECT-X VECTORIEL VECT-Y
        AS_ALLOCATE(vr=vecz, size=3)
        vecz(1)=vecx(1+1)*vecy(1+2)-vecy(1+1)*vecx(1+2)
        vecz(1+1)=vecx(1+2)*vecy(1)-vecy(1+2)*vecx(1)
        vecz(1+2)=vecx(1)*vecy(1+1)-vecy(1)*vecx(1+1)
        do 2 in = 1, 3
            dir(1,in)=vecx(in)
            dir(2,in)=vecy(in)
            dir(3,in)=vecz(in)
 2      continue
    else if (method(1:7).eq.'AU_YANG') then
        yang = .true.
        call getvr8(' ', 'VECT_X', nbval=0, nbret=nvecx)
        nvecx=-nvecx
        if (nvecx .gt. 0) then
            call getvr8(' ', 'VECT_X', nbval=nvecx, vect=dir(1, 1), nbret=nbid)
        endif
        call getvr8(' ', 'ORIG_AXE', nbval=0, nbret=nveco)
        nveco=-nveco
        if (nveco .gt. 0) then
            call getvr8(' ', 'ORIG_AXE', nbval=nveco, vect=dir(1, 2), nbret=nbid)
        endif
        if (nvecx .lt. 0 .or. nveco .lt. 0) then
            call utmess('F', 'MODELISA7_3')
        endif
    endif
!
! VALEURS NON DEPENDANTES DE LA FREQUENCE
!
10  continue
    if (vate(1) .eq. 'SPEC_CORR_CONV_3') then
        call accep2(base(1:8), nbm, pg, phi, sphi)
    else
        call accep1(base(1:8), ligrmo, nbm, dir, yang)
    endif
!
!
! CAS SPEC_CORR_CONV_1 ET 2
    mxval = nbm*(nbm+1)/2
    chnumi = nomres//'.NUMI'
    call wkvect(chnumi, 'G V I', mxval, lnumi)
    chnumj = nomres//'.NUMJ'
    call wkvect(chnumj, 'G V I', mxval, lnumj)
    chvale = nomres//'.VALE'
    call jecrec(chvale, 'G V R', 'NU', 'DISPERSE', 'VARIABLE',&
                mxval)
    chfreq = nomres//'.DISC'
    call wkvect(chfreq, 'G V R', nbpoin, lfreq)
!
    do 310 iff = 0, nbpoin-1
        f=finit+iff*df
        zr(lfreq+iff) = f
310  end do
!
!  POUR LE CAS SPEC_CORR_CONV_3
    if (vate(1) .eq. 'SPEC_CORR_CONV_3') then
! TABLE CONTENANT LES FONCTIONS DE FORME
        is=vate(2)
        do 320 iff = 0, nbpoin-1
            f=finit+iff*df
            zr(lfreq+iff) = f
            call evalis(is, pg, phi, sphi, f,&
                        iff, nomres)
320      continue
    else
        ij = 0
        do 220 im2 = 1, nbm
!
            do 210 im1 = im2, nbm
                ij = ij + 1
!
                zi(lnumi-1+ij) = im1
                zi(lnumj-1+ij) = im2
!
                call jecroc(jexnum(chvale, ij))
                if (im1 .eq. im2) then
                    nbabs = nbpoin
                else
                    nbabs = 2*nbpoin
                endif
!
                call jeecra(jexnum(chvale, ij), 'LONMAX', nbabs)
                call jeecra(jexnum(chvale, ij), 'LONUTI', nbabs)
                call jeveuo(jexnum(chvale, ij), 'E', lvale)
!
! BOUCLE SUR LES FREQUENCES ET REMPLISSAGE DU .VALE
! IE VALEURS DES INTERSPECTRS
!
                ier = 0
                do 201 iff = 0, nbpoin-1
                    f=finit+iff*df
                    if (f .gt. fcoup) then
                        prs = 0.d0
                    else if (vate(1).eq.'SPEC_CORR_CONV_2') then
                        puls = deuxpi*f
                        call fointe('F', fonct, 1, ['PULS'], [puls],&
                                    prs, ier)
                        call accept(f, nbm, method, im2, im1,&
                                    uflui, jc, dir, uc, ut,&
                                    long1, long2)
                    else
                        prs = dspprs(kste,uflui,dhyd,rho,f,fcoup)
                        call accept(f, nbm, method, im2, im1,&
                                    uflui, jc, dir, uc, ut,&
                                    long1, long2)
                    endif
                    if (im1 .eq. im2) then
                        zr(lvale+iff)=prs*jc
                    else
                        zr(lvale+2*iff)=prs*jc
                        zr(lvale+2*iff+1)=0.d0
                    endif
201              continue
!
210          continue
!
220      end do
!
    endif
!
    AS_DEALLOCATE(vr=vecx)
    AS_DEALLOCATE(vr=vecy)
    AS_DEALLOCATE(vr=vecz)

    if (vate(1) .eq. 'SPEC_CORR_CONV_3') then
    else
        call jedetc('V', '&&329', 1)
        call jedetc('V', '&&V.M', 1)
        call jedetc('V', '&&GROTAB.TAB', 1)
    endif
!
    call jedema()
end subroutine
