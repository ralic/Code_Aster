subroutine asstoc(mome, resu, nomsy, neq, repdir,&
                  ndir, comdir, typcdi, glob, prim)
    implicit none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rscrsd.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsexis.h'
    include 'asterfort/rsnoch.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/vtdefs.h'
    integer :: neq, ndir(*)
    real(kind=8) :: repdir(neq, *)
    logical :: comdir, glob, prim
    character(len=16) :: nomsy
    character(len=*) :: mome, resu, typcdi
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     COMMANDE : COMB_SISM_MODAL
!        STOCKAGE DES CHAMPS CALCULES
!     ------------------------------------------------------------------
! IN  : MOME   : MODES MECANIQUES
! IN  : RESU   : NOM UTILISATEUR DE LA COMMANDE
! IN  : NOMSY  : OPTION DE CALCUL
! IN  : NEQ    : NOMBRE D'EQUATIONS
! IN  : REPDIR : VECTEUR DES RECOMBINAISONS
! IN  : NDIR   : VECTEUR DES DIRECTIONS
! IN  : COMDIR : =.TRUE.  , COMBINAISON DES DIRECTIONS
!                =.FALSE. , PAS DE COMBINAISON DES DIRECTIONS
! IN  : TYPCDI : TYPE DE COMBINAISON DES DIRECTIONS
!     ------------------------------------------------------------------
    integer :: ibid, i, id, ieq, ier, in, iordr, jdef, jdir, jval, lvale, nbmode
    integer :: nbtrou, jdrr
    real(kind=8) :: r8b, r1, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19
    real(kind=8) :: r2, r20, r21, r22, r23, r24, r3, r4, r5, r6, r7, r8, r9, rx
    real(kind=8) :: ry, rz, xxx
    character(len=8) :: k8b, comp(5)
    character(len=16) :: noms2, concep, nomcmd, def
    character(len=19) :: moncha, champ
    character(len=24) :: vale
    character(len=24) :: valk(3)
    complex(kind=8) :: c16b
!     ------------------------------------------------------------------
    data  comp / 'X' , 'Y' , 'Z' , 'QUAD' , 'NEWMARK' /
    data  vale / '                   .VALE' /
!     ------------------------------------------------------------------
!
    call jemarq()
    call getres(k8b, concep, nomcmd)
    call getfac('DEPL_MULT_APPUI', nbmode)
!
    do 10 i = 1, 3
        if (ndir(i) .eq. 1) nbmode = nbmode + 1
10  end do
    if (comdir) nbmode = nbmode + 1
!
!     --- CREATION DE LA STRUCTURE D'ACCUEIL ---
    call rsexis(resu, ier)
    if (ier .eq. 0) call rscrsd('G', resu, concep, nbmode)
    noms2 = nomsy
    if (nomsy(1:4) .eq. 'VITE') noms2 = 'DEPL'
    if (nomsy(1:4) .eq. 'ACCE') noms2 = 'DEPL'
    call rsorac(mome, 'TOUT_ORDRE', ibid, r8b, k8b,&
                c16b, r8b, k8b, iordr, 1,&
                nbtrou)
    call rsexch('F', mome, noms2, iordr, moncha,&
                ier)
!
    iordr = 0
!
    if (glob) then
        def = 'GLOBALE'
    else if (prim) then
        def = 'PRIMAIRE'
    endif
    do 20 id = 1, 3
        if (ndir(id) .eq. 1) then
            iordr = iordr + 1
!
!           --- CHAMP RECOMBINE ---
            call rsexch(' ', resu, nomsy, iordr, champ,&
                        ier)
            if (ier .eq. 100) then
                call vtdefs(champ, moncha, 'G', 'R')
            else
                valk(1) = nomsy
                valk(2) = comp(id)
                valk(3) = champ
                call u2mesk('F', 'SEISME_26', 3, valk)
            endif
            vale(1:19) = champ
            call jeexin(vale(1:19)//'.VALE', ibid)
            if (ibid .gt. 0) then
                vale(20:24)='.VALE'
            else
                vale(20:24)='.CELV'
            endif
            call jeveuo(vale, 'E', jval)
!
            do 30 in = 1, neq
                zr(jval+in-1) = sqrt( abs ( repdir(in,id) ) )
30          continue
            call jelibe(vale)
            call rsnoch(resu, nomsy, iordr)
!
!           --- PARAMETRE ---
            call rsadpa(resu, 'E', 1, 'NOEUD_CMP', iordr,&
                        0, jdir, k8b)
            zk16(jdir) = 'DIR     '//comp(id)
            call rsadpa(resu, 'E', 1, 'TYPE_DEFO', iordr,&
                        0, jdef, k8b)
            zk16(jdef) = def
            call rsadpa(resu, 'E', 1, 'FREQ', iordr,&
                        0, jdrr, k8b)
            zr(jdrr) = id
        endif
20  end do
!
    if (comdir) then
        iordr = iordr + 1
!
!        --- CHAMP RECOMBINE ---
        call rsexch(' ', resu, nomsy, iordr, champ,&
                    ier)
        if (ier .eq. 100) then
            call vtdefs(champ, moncha, 'G', 'R')
        else
            valk(1) = nomsy
            valk(2) = comp(id)
            valk(3) = champ
            call u2mesk('F', 'SEISME_26', 3, valk)
        endif
        vale(1:19) = champ
        call jeexin(vale(1:19)//'.VALE', ibid)
        if (ibid .gt. 0) then
            vale(20:24)='.VALE'
        else
            vale(20:24)='.CELV'
        endif
        call jeveuo(vale, 'E', lvale)
!
        if (typcdi(1:4) .eq. 'QUAD') then
            do 40 ieq = 1, neq
                xxx = abs ( repdir(ieq,1) ) + abs ( repdir(ieq,2) ) + abs ( repdir(ieq,3) )
                zr(lvale+ieq-1) = sqrt( xxx )
40          continue
        else if (typcdi(1:4).eq.'NEWM') then
            do 42 ieq = 1, neq
                rx = sqrt( abs ( repdir(ieq,1) ) )
                ry = sqrt( abs ( repdir(ieq,2) ) )
                rz = sqrt( abs ( repdir(ieq,3) ) )
                r1 = rx + 0.4d0*ry + 0.4d0*rz
                r2 = 0.4d0*rx + ry + 0.4d0*rz
                r3 = 0.4d0*rx + 0.4d0*ry + rz
                r4 = rx - 0.4d0*ry + 0.4d0*rz
                r5 = 0.4d0*rx - ry + 0.4d0*rz
                r6 = 0.4d0*rx - 0.4d0*ry + rz
                r7 = rx - 0.4d0*ry - 0.4d0*rz
                r8 = 0.4d0*rx - ry - 0.4d0*rz
                r9 = 0.4d0*rx - 0.4d0*ry - rz
                r10 = rx + 0.4d0*ry - 0.4d0*rz
                r11 = 0.4d0*rx + ry - 0.4d0*rz
                r12 = 0.4d0*rx + 0.4d0*ry - rz
                r13 = -rx + 0.4d0*ry + 0.4d0*rz
                r14 = -0.4d0*rx + ry + 0.4d0*rz
                r15 = -0.4d0*rx + 0.4d0*ry + rz
                r16 = -rx - 0.4d0*ry + 0.4d0*rz
                r17 = -0.4d0*rx - ry + 0.4d0*rz
                r18 = -0.4d0*rx - 0.4d0*ry + rz
                r19 = -rx - 0.4d0*ry - 0.4d0*rz
                r20 = -0.4d0*rx - ry - 0.4d0*rz
                r21 = -0.4d0*rx - 0.4d0*ry - rz
                r22 = -rx + 0.4d0*ry - 0.4d0*rz
                r23 = -0.4d0*rx + ry - 0.4d0*rz
                r24 = -0.4d0*rx + 0.4d0*ry - rz
                xxx = max(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13, r14)
                xxx = max(xxx,r15,r16,r17,r18,r19,r20,r21,r22,r23,r24)
                zr(lvale+ieq-1) = xxx
42          continue
        endif
        call jelibe(vale)
        call rsnoch(resu, nomsy, iordr)
!
!        --- PARAMETRE ---
        call rsadpa(resu, 'E', 1, 'NOEUD_CMP', iordr,&
                    0, jdir, k8b)
        if (typcdi(1:4) .eq. 'QUAD') then
            zk16(jdir) = 'COMBI   '//comp(4)
        else if (typcdi(1:4).eq.'NEWM') then
            zk16(jdir) = 'COMBI   '//comp(5)
        endif
        call rsadpa(resu, 'E', 1, 'TYPE_DEFO', iordr,&
                    0, jdef, k8b)
        zk16(jdef) = def
        call rsadpa(resu, 'E', 1, 'FREQ', iordr,&
                    0, jdrr, k8b)
        zr(jdrr) = 4
    endif
!
    call jedema()
end subroutine
