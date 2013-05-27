subroutine pechli(resu, modele, mate)
    implicit   none
    include 'jeveux.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8miem.h'
    include 'asterfort/calcul.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jerecu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mecact.h'
    include 'asterfort/megeom.h'
    include 'asterfort/memaxm.h'
    include 'asterfort/mesomm.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsexpa.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    include 'asterfort/u2mesk.h'
    character(len=8) :: modele
    character(len=24) :: mate
    character(len=*) :: resu
! ----------------------------------------------------------------------
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
!
!     OPERATEUR   POST_ELEM
!
!     TRAITEMENT DU MOT CLE-FACTEUR "CHAR_LIMITE"
!
! ----------------------------------------------------------------------
!
    logical :: chrcst
    integer :: i, ibid, iret, jinst, jpilo
    integer :: nbord, jord, numord
    real(kind=8) :: chlim(3), chmax(3), inst, eta, prec, valer(3), f0u, m
    complex(kind=8) :: c16b
!
    character(len=8) :: crit, result, lpain(4), lpaout(1)
    character(len=8) :: k8b, typarr(4), chli(3)
    character(len=16) :: option, noparr(4)
    character(len=24) :: ligrmo, chgeom, depla, chtime
    character(len=24) :: lchin(4), lchout(1), lisord
    character(len=72) :: rep
    integer :: iarg
!
! ----------------------------------------------------------------------
!
!
    call jemarq()
    lisord = '&&PECHLI.VECTORDR'
    chtime = '&&PECHLI.CH_INST_R'
    f0u = 0
!
!
! -- VERIFICATIONS INITIALES
!
    call getvid(' ', 'RESULTAT', 0, iarg, 1,&
                result, iret)
    call rsexpa(result, 2, 'ETA_PILOTAGE', iret)
!
    if (iret .eq. 0) call u2mesk('F', 'POSTELEM_3', 1, result)
!
!
! -- EXISTENCE D'UN CHARGEMENT CONSTANT
    call getvtx('CHAR_LIMITE', 'CHAR_CSTE', 1, iarg, 1,&
                rep, iret)
    chrcst = rep .eq. 'OUI'
!
!
!
!
! -- ECRITURE DE LA TABLE RESULTAT
!
    typarr(1) = 'I'
    typarr(2) = 'R'
    typarr(3) = 'R'
    typarr(4) = 'R'
!
    noparr(1) = 'NUME_ORDRE'
    noparr(2) = 'INST'
    noparr(3) = 'CHAR_LIMI_SUP'
!
    if (chrcst) then
        noparr(4) = 'PUIS_CHAR_CSTE'
    else
        noparr(4) = 'CHAR_LIMI_ESTIM'
    endif
!
    call tbcrsd(resu, 'G')
    call tbajpa(resu, 4, noparr, typarr)
!
!
!
    call megeom(modele, chgeom)
    ligrmo = modele//'.MODELE'
!
!
! -- EXTRACTION DES NUMEROS D'ORDRE DU CALCUL
!
    call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                prec, iret)
    call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                crit, iret)
    call rsutnu(result, ' ', 0, lisord, nbord,&
                prec, crit, iret)
    if (iret .ne. 0) call u2mesk('F', 'POSTELEM_1', 1, result)
    call jeveuo(lisord, 'L', jord)
!
!
! -- CALCUL DES CHARGES LIMITES AUX DIFFERENTS INSTANTS
!
    do 10 i = 1, nbord
        call jemarq()
        call jerecu('V')
!
!      EXTRACTION DU CHAMP DE DEPLACEMENT
        numord = zi(jord-1+i)
        call rsexch('F', result, 'DEPL', numord, depla,&
                    iret)
!
!
!      CREACTION DE LA CARTE DE L INSTANT DE CALCUL
        call rsadpa(result, 'L', 1, 'INST', numord,&
                    0, jinst, k8b)
        inst = zr(jinst)
        call mecact('V', chtime, 'MODELE', ligrmo, 'INST_R',&
                    1, 'INST', ibid, inst, c16b,&
                    k8b)
!
!
!      CALCUL DES TERMES ELEMENTAIRES
        lpaout(1) = 'PECHLI'
        lchout(1) = '&&PECHLI'
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PDEPLAR'
        lchin(2) = depla
        lpain(3) = 'PMATERC'
        lchin(3) = mate
        lpain(4) = 'PTEMPSR'
        lchin(4) = chtime
        option = 'CHAR_LIMITE'
        call calcul('S', option, ligrmo, 4, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
!
!      SOMMATION DE TOUS LES TERMES ELEMENTAIRES
!       CHLIM(1) : CHAR_LIMI_SUP
!       CHLIM(2) : CHAR_LIMI_ESTIM
!       CHLIM(3) : MAX UTILE AU CALCUL DE CHAR_LIMI_ESTIM
!
        call mesomm(lchout(1), 3, ibid, chlim, c16b,&
                    0, ibid)
!
        chli(1) = 'CHLI1'
        chli(2) = 'CHLI2'
        chli(3) = 'CHLI3'
        call memaxm('MAX', lchout(1), 'CHLI3', 3, chli,&
                    chmax, 0, ibid)
!
!      CALCUL DU CHARGEMENT PERMANENT SI NECESSAIRE
        if (chrcst) then
            call rsadpa(result, 'L', 1, 'ETA_PILOTAGE', numord,&
                        0, jpilo, k8b)
            eta = zr(jpilo)
            m = 1 + 10** (1-inst)
            f0u = m*chlim(2) - eta
            chlim(1) = chlim(1) - f0u
        else
            if (chmax(3) .le. r8miem()) then
                chlim(2) = 0.0d0
            else
                chlim(2) = chlim(2)/chmax(3)
            endif
        endif
!
!
!      ECRITURE DANS LA TABLE RESU DE LA CHARGE LIMITE
        valer(1) = inst
        valer(2) = chlim(1)
        if (chrcst) then
            valer(3) = f0u
        else
            valer(3) = chlim(2)
        endif
        call tbajli(resu, 4, noparr, numord, valer,&
                    c16b, k8b, 0)
!
        call jedema()
10  end do
!
! --- MENAGE
    call jedetr('&&PECHLI.VECTORDR')
    call detrsd('CARTE', '&&PECHLI.CH_INST_R')
!
    call jedema()
end subroutine
