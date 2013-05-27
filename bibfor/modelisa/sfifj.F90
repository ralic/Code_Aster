subroutine sfifj(nomres)
    implicit   none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
!
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/r8prem.h'
    include 'asterfort/accep1.h'
    include 'asterfort/accep2.h'
    include 'asterfort/accept.h'
    include 'asterfort/chpver.h'
    include 'asterfort/dspprs.h'
    include 'asterfort/evalis.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetc.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nfinit, nfin, nbm, nbpoin, nbid, iarg
    integer :: npoin, iff, ivare, lvale, ibid, in
    integer :: im1, im2, ivate, ivecx, ivecy, ivecz, nvecx, nvecy
    integer :: nveco, ier, ncham, jpara, jordr
    integer :: lnumi, lnumj, lfreq, mxval, nbabs, ij
    real(kind=8) :: fmin, fmax, finit, ffin, df, f, prs
    real(kind=8) :: kste, uflui, dhyd, rho, rbid, jc, fcoupu, fmodel
    real(kind=8) :: dir(3, 3), fcoup
    real(kind=8) :: deuxpi, puls, uc, ut, long1, long2
    real(kind=8) :: valr
    character(len=8) :: k8b, nomres, is
    character(len=8) :: spectr, method
    character(len=19) :: base, fonct, chamno, pg, phi, sphi
    character(len=24) :: ligrmo
    character(len=24) :: chnumi, chnumj, chfreq, chvale
    logical :: yang
!
    data         deuxpi/6.28318530718D0/,yang/.false./
!
!-----------------------------------------------------------------------
    call jemarq()
!
! RECHERCHE DE LA PRESENCE D'UN CHAMNO
    call getvid(' ', 'CHAM_NO', 0, iarg, 0,&
                chamno, ncham)
!
    if (ncham .eq. 0) then
!
! RECUPERATION DE LA BASE MODALE
        call getvid(' ', 'MODE_MECA', 0, iarg, 1,&
                    base, ibid)
!
        call jeveuo(base//'.ORDR', 'L', jordr)
        call jelira(base//'.ORDR', 'LONUTI', nbm, k8b)
!
! RECUPERATION DE LA FREQUENCE MINIMALE ET MAX DES MODES
!
        call rsadpa(base, 'L', 1, 'FREQ', zi(jordr-1+1),&
                    0, jpara, k8b)
        fmin = zr(jpara)
        call rsadpa(base, 'L', 1, 'FREQ', zi(jordr-1+nbm),&
                    0, jpara, k8b)
        fmax = zr(jpara)
    else
        call getvid(' ', 'CHAM_NO', 0, iarg, 1,&
                    chamno, ibid)
        call chpver('F', chamno, 'NOEU', 'DEPL_R', ier)
        call getvid(' ', 'CHAM_NO', 0, iarg, 0,&
                    k8b, ncham)
        nbm = -ncham
    endif
!
! RECUPERATION DE LA FREQUENCE MINIMALE ET MAX DE LA PLAGE
! DE FREQUENCE ETUDIEE
!
    call getvr8(' ', 'FREQ_INIT', 0, iarg, 1,&
                finit, nfinit)
    call getvr8(' ', 'FREQ_FIN', 0, iarg, 1,&
                ffin, nfin)
    if ((ffin-finit) .lt. r8prem()) then
        call u2mess('F', 'MODELISA6_97')
    endif
!
    if (nfinit .lt. 0) then
        if (ncham .ne. 0) call u2mess('F', 'MODELISA6_98')
        valr = fmin
        call u2mesg('I', 'MODELISA9_15', 0, ' ', 0,&
                    0, 1, valr)
        finit=fmin
    endif
    if (nfin .lt. 0) then
        if (ncham .ne. 0) call u2mess('F', 'MODELISA6_99')
        valr = fmax
        call u2mesg('I', 'MODELISA9_16', 0, ' ', 0,&
                    0, 1, valr)
        ffin=fmax
    endif
!
! DISCRETISATION FREQUENTIELLE
    call getvis(' ', 'NB_POIN', 0, iarg, 1,&
                nbpoin, npoin)
!
! PAS FREQUENTIEL
    df = (ffin-finit) / (nbpoin-1)
    if (df .lt. r8prem()) then
        call u2mess('F', 'MODELISA7_1')
    endif
!
! CALCUL DE L'ACCEPTANCE
!
    call getvid(' ', 'SPEC_TURB', 0, iarg, 1,&
                spectr, ibid)
    call jeveuo(spectr//'           .VARE', 'L', ivare)
    call jeveuo(spectr//'           .VATE', 'L', ivate)
!
! RECUPERATION DES CONSTANTES DU SPECTRES DU
! MODELE 5 : CONSTANT PUIS NUL POUR FR > 10
!
    if (zk16(ivate) .eq. 'SPEC_CORR_CONV_1') then
        uflui = zr(ivare+2)
        rho = zr(ivare+3)
        fcoupu = zr(ivare+4)
        kste = zr(ivare+5)
        dhyd = zr(ivare+6)
! LONGUEURS DE CORRELATION
        long1=zr(ivare)
        long2=zr(ivare+1)
! VITESSE CONVECTIVE RADIALE (METHODE AU-YANG)
        uc=zr(ivare+7)*uflui
! VITESSE CONVECTIVE ORTHORADIALE (METHODE AU-YANG)
        ut=zr(ivare+8)*uflui
!
! CALCUL DE LA FREQUENCE DE COUPURE PRONE PAR LE MODELE
! ET COMPARAISON AVEC LA FREQUENCE DE COUPURE DONNEE PAR
! L UTILISATEUR
!
        fmodel = 10.d0 * uflui / dhyd
        if (fcoupu .le. fmodel) then
            valr = fcoupu
            call u2mesg('I', 'MODELISA9_17', 0, ' ', 0,&
                        0, 1, valr)
            valr = fmodel
            call u2mesg('I', 'MODELISA9_18', 0, ' ', 0,&
                        0, 1, valr)
            call u2mesg('I', 'MODELISA9_19', 0, ' ', 0,&
                        0, 0, 0.d0)
            fcoup = fcoupu * dhyd / uflui
        else
            valr = fcoupu
            call u2mesg('I', 'MODELISA9_20', 0, ' ', 0,&
                        0, 1, valr)
            valr = fmodel
            call u2mesg('I', 'MODELISA9_21', 0, ' ', 0,&
                        0, 1, valr)
            call u2mesg('I', 'MODELISA9_22', 0, ' ', 0,&
                        0, 0, 0.d0)
            fcoup = 10.d0
        endif
!
! RECUPERATION DE LA METHOD DE LA FONCTION
! DE COHERENCE
!
        method = zk16(ivate+10)(1:8)
    else if (zk16(ivate).eq.'SPEC_CORR_CONV_2') then
        uflui=zr(ivare)
        fcoup=zr(ivare+1)
        method=zk16(ivate+4)(1:8)
        fonct =zk16(ivate+1)
    else if (zk16(ivate).eq.'SPEC_CORR_CONV_3') then
        fonct =zk16(ivate+1)
        goto 10
    endif
!
!
! RECUPERATION DES DIRECTIONS DU PLAN DE LA PLANCHE
    if (method(1:6) .eq. 'CORCOS') then
        call getvr8(' ', 'VECT_X', 0, iarg, 0,&
                    rbid, nvecx)
        nvecx=-nvecx
        if (nvecx .gt. 0) then
            call wkvect('&&SFIFJ.VECX', 'V V R', 3, ivecx)
            call getvr8(' ', 'VECT_X', 0, iarg, nvecx,&
                        zr(ivecx), nbid)
        endif
        call getvr8(' ', 'VECT_Y', 0, iarg, 0,&
                    rbid, nvecy)
        nvecy=-nvecy
        if (nvecy .gt. 0) then
            call wkvect('&&SFIFJ.VECY', 'V V R', 3, ivecy)
            call getvr8(' ', 'VECT_Y', 0, iarg, nvecy,&
                        zr(ivecy), nbid)
        endif
        if (nvecx .lt. 0 .or. nvecy .lt. 0) call u2mess('F', 'MODELISA7_2')
!
! VECTEUR Z LOCAL = VECT-X VECTORIEL VECT-Y
        call wkvect('&&SFIFJ.VECZ', 'V V R', 3, ivecz)
        zr(ivecz)=zr(ivecx+1)*zr(ivecy+2)-zr(ivecy+1)*zr(ivecx+2)
        zr(ivecz+1)=zr(ivecx+2)*zr(ivecy)-zr(ivecy+2)*zr(ivecx)
        zr(ivecz+2)=zr(ivecx)*zr(ivecy+1)-zr(ivecy)*zr(ivecx+1)
        do 2 in = 1, 3
            dir(1,in)=zr(ivecx+in-1)
            dir(2,in)=zr(ivecy+in-1)
            dir(3,in)=zr(ivecz+in-1)
 2      continue
    else if (method(1:7).eq.'AU_YANG') then
        yang = .true.
        call getvr8(' ', 'VECT_X', 0, iarg, 0,&
                    rbid, nvecx)
        nvecx=-nvecx
        if (nvecx .gt. 0) call getvr8(' ', 'VECT_X', 0, iarg, nvecx,&
                                      dir(1, 1), nbid)
        call getvr8(' ', 'ORIG_AXE', 0, iarg, 0,&
                    rbid, nveco)
        nveco=-nveco
        if (nveco .gt. 0) call getvr8(' ', 'ORIG_AXE', 0, iarg, nveco,&
                                      dir(1, 2), nbid)
        if (nvecx .lt. 0 .or. nveco .lt. 0) call u2mess('F', 'MODELISA7_3')
    endif
!
! VALEURS NON DEPENDANTES DE LA FREQUENCE
!
10  continue
    if (zk16(ivate) .eq. 'SPEC_CORR_CONV_3') then
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
    chfreq = nomres//'.FREQ'
    call wkvect(chfreq, 'G V R', nbpoin, lfreq)
!
    do 310 iff = 0, nbpoin-1
        f=finit+iff*df
        zr(lfreq+iff) = f
310  end do
!
!  POUR LE CAS SPEC_CORR_CONV_3
    if (zk16(ivate) .eq. 'SPEC_CORR_CONV_3') then
! TABLE CONTENANT LES FONCTIONS DE FORME
        is=zk16(ivate+1)
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
                call jeecra(jexnum(chvale, ij), 'LONMAX', nbabs, ' ')
                call jeecra(jexnum(chvale, ij), 'LONUTI', nbabs, ' ')
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
                    else if (zk16(ivate).eq.'SPEC_CORR_CONV_2') then
                        puls = deuxpi*f
                        call fointe('F', fonct, 1, 'PULS', puls,&
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
    if (zk16(ivate) .eq. 'SPEC_CORR_CONV_3') then
        call jedetr('&&SFIFJ.VECX')
        call jedetr('&&SFIFJ.VECY')
        call jedetr('&&SFIFJ.VECZ')
    else
        call jedetc('V', '&&329', 1)
        call jedetc('V', '&&V.M', 1)
        call jedetc('V', '&&GROTAB.TAB', 1)
    endif
!
    call jedema()
end subroutine
