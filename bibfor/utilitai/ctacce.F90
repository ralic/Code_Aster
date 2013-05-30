subroutine ctacce(nsymb, typac, nbval, nival, nrval,&
                  niord, nkcha, resu)
    implicit   none
    include 'jeveux.h'
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    integer :: nbval
    character(len=24) :: nival, nrval, niord, nkcha
    character(len=8) :: typac, resu
    character(len=16) :: nsymb
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
!     ----- OPERATEUR CREA_TABLE , MOT-CLE FACTEUR RESU   --------------
!
!        BUT : RECUPERER LES ACCES DE LA SD RESULTATS
!
!        IN/OUT : NIVAL (K24): OBJET DES VALEURS D'ACCES (ENTIERS)
!                 NRVAL (K24): OBJET DES VALEURS D'ACCES (REELS)
!                 NIORD (K24): OBJET DES NUMEROS D'ORDRE
!                 NKCHA (K24): OBJET DES NOMS DE CHAMP
!           OUT : RESU  (K8) : NOM DU RESULTAT (SI RESULTAT, SINON ' ')
!                 TYPAC (K8) : ACCES (ORDRE,MODE,FREQ,INST)
!                 NSYMB (K16): NOM SYMBOLIQUE DU CHAMP
!                 NBVAL (I)  : NOMBRE DE VALEURS D'ACCES
!
! ----------------------------------------------------------------------
    character(len=16) :: concep
    character(len=8) :: k8b
    integer :: n0, n1, jkcha, jrval, jival, jniord, nbto, nbno, nblo, nbni, nbli
    integer :: nbnm, nblm, nbnf, nblf, nbis, nbr8, jlist, ibid, nbtrou, i
    integer :: vali, n2, jinst, kk, nuord
    real(kind=8) :: r8b, epsi, valr, rinst
    complex(kind=8) :: cbid
    character(len=8) :: crit
    character(len=16) :: valk
    character(len=24) :: nlist
    integer :: iarg
!     ------------------------------------------------------------------
    call jemarq()
!
    call getvid('RESU', 'CHAM_GD', 1, iarg, 0,&
                k8b, n0)
    call getvid('RESU', 'RESULTAT', 1, iarg, 0,&
                k8b, n1)
!
! =============================================================
! -1- CAS: CHAM_GD
! =============================================================
!
    if (n0 .ne. 0) then
!
        call wkvect(nkcha, 'V V K24', 1, jkcha)
        call getvid('RESU', 'CHAM_GD', 1, iarg, 1,&
                    zk24(jkcha), n0)
        call wkvect(nrval, 'V V R', 1, jrval)
        zr(jrval) = 0.0d0
        call wkvect(nival, 'V V I', 1, jival)
        zi(jival) = 0
        call wkvect(niord, 'V V I', 1, jniord)
        zi(jniord)=0
        nbval=1
        typac = ' '
        nsymb = ' '
        resu = ' '
!
! =============================================================
! -2- CAS: RESULTAT/NOM_CHAM
! =============================================================
    else if (n1.ne.0) then
!
! --- 2.1- ON DETERMINE :
!     ------------------
!         NBVAL = NOMBRE DE VALEURS D'ACCES
!         TYPAC = TYPE D'ACCES (ORDRE,INST,FREQ,MODE,TOUT)
!         NIVAL = TABLEAU DES VALEURS D'ACCES (ENTIERES)
!         NRVAL = TABLEAU DES VALEURS D'ACCES (REELLES)
!         NKCHA = TABLEAU DES NOMS DE CHAMP
!
        call getvid('RESU', 'RESULTAT', 1, iarg, 1,&
                    resu, n1)
!
        call getvr8('RESU', 'PRECISION', 1, iarg, 1,&
                    epsi, n1)
        call getvtx('RESU', 'CRITERE', 1, iarg, 1,&
                    crit, n1)
        call getvtx('RESU', 'TOUT_ORDRE', 1, iarg, 0,&
                    k8b, nbto)
        call getvis('RESU', 'NUME_ORDRE', 1, iarg, 0,&
                    ibid, nbno)
        call getvid('RESU', 'LIST_ORDRE', 1, iarg, 0,&
                    k8b, nblo)
        call getvr8('RESU', 'INST', 1, iarg, 0,&
                    r8b, nbni)
        call getvid('RESU', 'LIST_INST', 1, iarg, 0,&
                    k8b, nbli)
        call getvis('RESU', 'MODE', 1, iarg, 0,&
                    ibid, nbnm)
        call getvid('RESU', 'LIST_MODE', 1, iarg, 0,&
                    k8b, nblm)
        call getvr8('RESU', 'FREQ', 1, iarg, 0,&
                    r8b, nbnf)
        call getvid('RESU', 'LIST_FREQ', 1, iarg, 0,&
                    k8b, nblf)
!
        nbto=-nbto
        nbno=-nbno
        nblo=-nblo
        nbni=-nbni
        nbli=-nbli
        nbnm=-nbnm
        nblm=-nblm
        nbnf=-nbnf
        nblf=-nblf
        nbis=nbno+nblo+nbnm+nblm
        nbr8=nbni+nbli+nbnf+nblf
!
!    -- ACCES PAR ORDRE, MODE, LIST_ORDRE, LIST_MODE :
        if (nbis .ne. 0) then
            call wkvect(nrval, 'V V R', 1, jrval)
            zr(jrval) = 0.0d0
!        -- NUME_ORDRE
            if (nbno .ne. 0) then
                typac = 'ORDRE'
                nbval = nbno
                call wkvect(nival, 'V V I', nbno, jival)
                call getvis('RESU', 'NUME_ORDRE', 1, iarg, nbno,&
                            zi(jival), n1)
!        -- MODE
            else if (nbnm .ne. 0) then
                typac = 'MODE'
                nbval = nbnm
                call wkvect(nival, 'V V I', nbnm, jival)
                call getvis('RESU', 'MODE', 1, iarg, nbnm,&
                            zi(jival), n1)
!        -- LIST_ORDRE, LIST_MODE
            else if (nblo .ne. 0) then
                if (nblo .ne. 0) then
                    typac = 'ORDRE'
                    call getvid('RESU', 'LIST_ORDRE', 1, iarg, 1,&
                                nlist, n1)
                else
                    typac = 'MODE'
                    call getvid('RESU', 'LIST_MODE', 1, iarg, 1,&
                                nlist, n1)
                endif
                nlist(20:24) = '.VALE'
                call jelira(nlist, 'LONMAX', nbval, k8b)
                call jeveuo(nlist, 'L', jlist)
                call wkvect(nival, 'V V I', nbval, jival)
                do 10 i = 1, nbval
                    zi(jival+i-1) = zi(jlist+i-1)
10              continue
            endif
!
!    -- ACCES PAR INST, FREQ, LIST_INST, LIST_FREQ :
        else if (nbr8 .ne. 0) then
            call wkvect(nival, 'V V I', 1, jival)
            zi(jival) = 0
!        -- INST
            if (nbni .ne. 0) then
                typac = 'INST'
                nbval = nbni
                call wkvect(nrval, 'V V R', nbni, jrval)
                call getvr8('RESU', 'INST', 1, iarg, nbni,&
                            zr(jrval), n1)
!        -- FREQ
            else if (nbnf .ne. 0) then
                typac = 'FREQ'
                nbval = nbnf
                call wkvect(nrval, 'V V R', nbnf, jrval)
                call getvr8('RESU', 'FREQ', 1, iarg, nbnf,&
                            zr(jrval), n1)
            else
                if (nbli .ne. 0) then
                    typac = 'INST'
                    call getvid('RESU', 'LIST_INST', 1, iarg, 1,&
                                nlist, n1)
                else
                    typac = 'FREQ'
                    call getvid('RESU', 'LIST_FREQ', 1, iarg, 1,&
                                nlist, n1)
                endif
                nlist(20:24) = '.VALE'
                call jelira(nlist, 'LONMAX', nbval, k8b)
                call jeveuo(nlist, 'L', jlist)
                call wkvect(nrval, 'V V R', nbval, jrval)
                do 20 i = 1, nbval
                    zr(jrval+i-1)=zr(jlist+i-1)
20              continue
            endif
!
!    -- ACCES TOUT_ORDRE :
        else
!
            cbid = dcmplx(0,0)
            call rsorac(resu, 'LONUTI', ibid, r8b, k8b,&
                        cbid, r8b, k8b, nbval, 1,&
                        nbtrou)
            call wkvect(nival, 'V V I', nbval, jival)
            call rsorac(resu, 'TOUT_ORDRE', ibid, r8b, k8b,&
                        cbid, r8b, k8b, zi(jival), nbval,&
                        nbtrou)
!
!         -- SI LE RESULTAT EST UN EVOL_XXX, ON FORCE TYPAC='INST'
!            POUR QUE LES INSTANTS SOIENT IMPRIMES DANS LA TABLE :
            call gettco(resu, concep)
            if (concep(1:5) .eq. 'EVOL_') then
                typac='INST'
                call wkvect(nrval, 'V V R', nbval, jrval)
                do 25, kk=1,nbval
                nuord=zi(jival-1+kk)
                call rsadpa(resu, 'L', 1, 'INST', nuord,&
                            0, jinst, k8b)
                rinst=zr(jinst)
                zr(jrval-1+kk) = rinst
25              continue
            else
                typac = 'ORDRE'
                call wkvect(nrval, 'V V R', 1, jrval)
                zr(jrval) = 0.0d0
            endif
!
        endif
!
!
! --- 2.2- ON DETERMINE :
!     ------------------
!         NKCHA = TABLEAU DES NOMS DE CHAMP
!
        call getvtx('RESU', 'NOM_CHAM', 1, iarg, 1,&
                    nsymb, n1)
!
        call wkvect(niord, 'V V I', nbval, jniord)
        zi(jniord)=-1
        call wkvect(nkcha, 'V V K24', nbval, jkcha)
!
        if (typac .eq. 'ORDRE') then
            call jeveuo(nival, 'L', jival)
            do 30 i = 1, nbval
                zi(jniord+i-1)=zi(jival+i-1)
                call rsexch(' ', resu, nsymb, zi(jival+i-1), zk24(jkcha+i- 1),&
                            n1)
                if (n1 .ne. 0) then
                    valk = nsymb
                    vali = zi(jival+i-1)
                    call u2mesg('I', 'TABLE0_38', 1, valk, 1,&
                                vali, 0, 0.d0)
                    zk24(jkcha+i-1) = '&&CHAMP_INEXISTANT'
                endif
30          continue
!
        else if (typac .eq. 'MODE') then
            call jeveuo(nival, 'L', jival)
            do 40 i = 1, nbval
                call rsorac(resu, 'NUME_MODE', zi(jival+i-1), 0.0d0, k8b,&
                            cbid, epsi, crit, ibid, 0,&
                            n1)
                if (n1 .eq. 0) then
                    zk24(jkcha+i-1) = '&&CHAMP_INEXISTANT'
                    valk = typac
                    vali = zi(jival+i-1)
                    zi(jniord+i-1)=-1
                    call u2mesg('I', 'TABLE0_39', 1, valk, 1,&
                                vali, 0, 0.d0)
                    goto 40
                endif
                n1=-n1
                call rsorac(resu, 'NUME_MODE', zi(jival+i-1), 0.0d0, k8b,&
                            cbid, epsi, crit, zi(jniord+i-1), n1,&
                            n2)
                call rsexch(' ', resu, nsymb, zi(jniord+i-1), zk24(jkcha+ i-1),&
                            n2)
                if (n2 .ne. 0) then
                    valk = nsymb
                    vali = zi(jival+i-1)
                    zi(jniord+i-1)=-1
                    call u2mesg('I', 'TABLE0_38', 1, valk, 1,&
                                vali, 0, 0.d0)
                    zk24(jkcha+i-1) = '&&CHAMP_INEXISTANT'
                endif
40          continue
!
        else if (typac .eq. 'INST' .or. typac .eq. 'FREQ') then
            call jeveuo(nrval, 'L', jrval)
            do 50 i = 1, nbval
                call rsorac(resu, typac, 0, zr(jrval+i-1), k8b,&
                            cbid, epsi, crit, ibid, 0,&
                            n1)
                if (n1 .eq. 0) then
                    zk24(jkcha+i-1) = '&&CHAMP_INEXISTANT'
                    valk = typac
                    valr = zr(jrval+i-1)
                    zi(jniord+i-1)=-1
                    call u2mesg('I', 'TABLE0_40', 1, valk, 0,&
                                ibid, 1, valr)
                    goto 50
                endif
                n1=-n1
                if (n1 .gt. 1) then
                    valk = typac
                    call u2mesg('F', 'TABLE0_46', 1, valk, 0,&
                                ibid, 1, zr( jrval+i-1))
                endif
                call rsorac(resu, typac, 0, zr(jrval+i-1), k8b,&
                            cbid, epsi, crit, zi(jniord+i-1), n1,&
                            n2)
                call rsexch(' ', resu, nsymb, zi(jniord+i-1), zk24(jkcha+ i-1),&
                            n2)
                if (n2 .ne. 0) then
                    valk = nsymb
                    vali = zi(jniord+i-1)
                    call u2mesg('I', 'TABLE0_38', 1, valk, 1,&
                                vali, 0, 0.d0)
                    zk24(jkcha+i-1) = '&&CHAMP_INEXISTANT'
                endif
50          continue
        endif
    endif
!
    call jedema()
!
end subroutine
