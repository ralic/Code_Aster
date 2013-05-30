subroutine cazocp(char)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: char
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! LECTURE DES PARAMETRES PRINCIPAUX QUI NE DEPENDENT PAS DE LA ZONE
! DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: defico
    character(len=24) :: paracr, paraci
    integer :: jparcr, jparci
    integer :: nbreac, lgbloc, gcpmax, premax
    integer :: reacca, reacbs, reacbg
    character(len=16) :: rech, prec, reac, typcon, isto
    character(len=16) :: algoco, algofr, algoge
    integer :: noc
    real(kind=8) :: precis, coefrs
    real(kind=8) :: resige, resifr
    logical :: lgcp
    logical :: lctcd, lctcc, lxfcm, lfrot, lmail
    character(len=16) :: lissa
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    reac = 'AUTOMATIQUE'
    algoco = ' '
    algofr = ' '
    algoge = ' '
    nbreac = 2
    lgbloc = 10
    resige = 1.d-2
    resifr = 1.d-2
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    paracr = defico(1:16)//'.PARACR'
    paraci = defico(1:16)//'.PARACI'
    call jeveuo(paracr, 'E', jparcr)
    call jeveuo(paraci, 'E', jparci)
!
! --- DRAPEAUX
!
    lctcd = cfdisl(defico,'FORMUL_DISCRETE')
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    lmail = lctcd.or.lctcc
    lxfcm = cfdisl(defico,'FORMUL_XFEM')
    lgcp = cfdisl(defico,'CONT_GCP' )
    lfrot = cfdisl(defico,'FROTTEMENT')
!
! --- ALGORITHME GEOMETRIE
!
    if (lctcc) then
        call getvtx(' ', 'ALGO_RESO_GEOM', 1, iarg, 1,&
                    algoge, noc)
    else if (lxfcm) then
        algoge = 'POINT_FIXE'
    else if (lctcd) then
        algoge = 'POINT_FIXE'
    else
        call assert(.false.)
    endif
!
    if (algoge .eq. 'POINT_FIXE') then
        zi(jparci+9-1) = 0
    else if (algoge.eq.'NEWTON') then
        zi(jparci+9-1) = 1
    else
        call assert(.false.)
    endif
!
! --- PARAMETRES BOUCLE GEOMETRIQUE
!
    if (algoge .eq. 'POINT_FIXE') then
        call getvtx(' ', 'REAC_GEOM', 1, iarg, 1,&
                    reac, noc)
        if (reac .eq. 'SANS') then
            zi(jparci+1-1) = 0
            zr(jparcr+1-1) = resige
        else if (reac .eq. 'AUTOMATIQUE') then
            zi(jparci+1-1) = -1
            call getvis(' ', 'ITER_GEOM_MAXI', 1, iarg, 1,&
                        reacbg, noc)
            zi(jparci+6-1) = reacbg
            call getvr8(' ', 'RESI_GEOM', 1, iarg, 1,&
                        resige, noc)
            zr(jparcr+1-1) = resige
        else if (reac .eq. 'CONTROLE') then
            call getvis(' ', 'NB_ITER_GEOM', 1, iarg, 1,&
                        nbreac, noc)
            zi(jparci+1-1) = nbreac
            zr(jparcr+1-1) = resige
        else
            call assert(.false.)
        endif
    else if (algoge .eq. 'NEWTON') then
        call getvr8(' ', 'RESI_GEOM', 1, iarg, 1,&
                    resige, noc)
        zi(jparci+1-1) = 0
        zr(jparcr+1-1) = resige
    else
        call assert(.false.)
    endif
!
! --- ALGORITHMES FROTTEMENT
!
    if (lfrot) then
        if (lctcc) then
            call getvtx(' ', 'ALGO_RESO_FROT', 1, iarg, 1,&
                        algofr, noc)
        else if (lxfcm) then
            if (zi(jparci+1-1) .eq. 0) then
                algofr = 'POINT_FIXE'
            else
                algofr = 'NEWTON'
            endif
        else if (lctcd) then
            algofr = 'POINT_FIXE'
        else
            call assert(.false.)
        endif
    endif
!
    if (lfrot) then
        if (algofr .eq. 'POINT_FIXE') then
            zi(jparci+28-1) = 0
        else if (algofr.eq.'NEWTON') then
            zi(jparci+28-1) = 1
        else
            call assert(.false.)
        endif
    endif
!
! --- PARAMETRES BOUCLE FROTTEMENT
!
    if (lfrot) then
        if (lctcc) then
            if (algofr .eq. 'POINT_FIXE') then
                call getvis(' ', 'ITER_FROT_MAXI', 1, iarg, 1,&
                            reacbs, noc)
                zi(jparci+7-1) = reacbs
                call getvr8(' ', 'RESI_FROT', 1, iarg, 1,&
                            resifr, noc)
                zr(jparcr+2-1) = resifr
            else
                call getvr8(' ', 'RESI_FROT', 1, iarg, 1,&
                            resifr, noc)
                zr(jparcr+2-1) = resifr
            endif
        else if (lxfcm) then
            call getvis(' ', 'ITER_FROT_MAXI', 1, iarg, 1,&
                        reacbs, noc)
            zi(jparci+7-1) = reacbs
            call getvr8(' ', 'RESI_FROT', 1, iarg, 1,&
                        resifr, noc)
            zr(jparcr+2-1) = resifr
        endif
    else
        zi(jparci+20-1) = 0
    endif
!
! --- ALGORITHME CONTACT
!
    if (lctcc) then
        call getvtx(' ', 'ALGO_RESO_CONT', 1, iarg, 1,&
                    algoco, noc)
    else if (lxfcm) then
        algoco = 'POINT_FIXE'
    else if (lctcd) then
        algoco = 'POINT_FIXE'
    else
        call assert(.false.)
    endif
!
    if (algoco .eq. 'POINT_FIXE') then
        zi(jparci+27-1) = 0
    else if (algoco.eq.'NEWTON') then
        zi(jparci+27-1) = 1
    else
        call assert(.false.)
    endif
!
! --- PARAMETRES BOUCLE CONTACT
!
    if (algoco .eq. 'POINT_FIXE') then
        if (lxfcm .or. lctcc) then
            call getvis(' ', 'ITER_CONT_MULT', 1, iarg, 1,&
                        reacca, noc)
            call getvtx(' ', 'ITER_CONT_TYPE', 1, iarg, 1,&
                        typcon, noc)
            if (typcon .eq. 'MULT') then
                reacca = 4
                call getvis(' ', 'ITER_CONT_MULT', 1, iarg, 1,&
                            reacca, noc)
                zi(jparci+5-1) = reacca
                zi(jparci+10-1) = -1
            else if (typcon.eq.'MAXI') then
                reacca = 30
                call getvis(' ', 'ITER_CONT_MAXI', 1, iarg, 1,&
                            reacca, noc)
                zi(jparci+10-1) = reacca
                zi(jparci+5-1) = -1
            else
                call assert(.false.)
            endif
        else if (lctcd) then
            call getvis(' ', 'ITER_CONT_MULT', 1, iarg, 1,&
                        reacca, noc)
            zi(jparci+5-1) = reacca
            zi(jparci+10-1) = -1
        else
            call assert(.false.)
        endif
    else if (algoco.eq.'NEWTON') then
! PAS DE PARAMETRES
    else
        call assert(.false.)
    endif
!
!
! --- FORMULATION DISCRETE
!
    if (lctcd) then
! ---   ARRET OU PAS SI MATRICE DE CONTACT SINGULIERE
        call getvtx(' ', 'STOP_SINGULIER', 1, iarg, 1,&
                    isto, noc)
        if (isto .eq. 'OUI') then
            zi(jparci+2-1) = 0
        else if (isto .eq. 'NON') then
            zi(jparci+2-1) = 1
        else
            call assert(.false.)
        endif
! ---   NOMBRE DE PAQUETS POUR LA RESOLUTION DES SYSTEMES LINEAIRES
        call getvis(' ', 'NB_RESOL', 1, iarg, 1,&
                    lgbloc, noc)
        zi(jparci+3-1) = lgbloc
!
! --- PARAMETRE GCP
!
        if (lgcp) then
            call getvr8(' ', 'RESI_ABSO', 1, iarg, 1,&
                        precis, noc)
            if (noc .eq. 0) then
                call u2mess('F', 'CONTACT_4')
            endif
            zr(jparcr+4-1) = precis
!
! ---     NON UTILISE
            zi(jparci+11-1) = 0
!
            call getvis(' ', 'ITER_GCP_MAXI', 1, iarg, 1,&
                        gcpmax, noc)
            zi(jparci+12-1) = gcpmax
!
            call getvtx(' ', 'PRE_COND', 1, iarg, 1,&
                        prec, noc)
            if (prec .eq. 'SANS') then
                zi(jparci+13-1) = 0
            else if (prec.eq.'DIRICHLET') then
                zi(jparci+13-1) = 1
                call getvr8(' ', 'COEF_RESI', 1, iarg, 1,&
                            coefrs, noc)
                zr(jparcr+5-1) = coefrs
                call getvis(' ', 'ITER_PRE_MAXI', 1, iarg, 1,&
                            premax, noc)
                zi(jparci+14-1) = premax
            else
                call assert(.false.)
            endif
!
            call getvtx(' ', 'RECH_LINEAIRE', 1, iarg, 1,&
                        rech, noc)
            if (rech .eq. 'ADMISSIBLE') then
                zi(jparci+15-1) = 0
            else if (rech.eq.'NON_ADMISSIBLE') then
                zi(jparci+15-1) = 1
            else
                call assert(.false.)
            endif
        endif
    endif
!
! --- LISSAGE
!
    if (lmail) then
        call getvtx(' ', 'LISSAGE', 1, iarg, 1,&
                    lissa, noc)
        if (lissa(1:3) .eq. 'NON') then
            zi(jparci+19-1) = 0
        else if (lissa(1:3) .eq. 'OUI') then
            zi(jparci+19-1) = 1
        else
            call assert(.false.)
        endif
    endif
!
! --- METHODE VERIF
!
    if (lmail) then
        call getvtx(' ', 'STOP_INTERP', 1, iarg, 1,&
                    isto, noc)
        if (isto .eq. 'OUI') then
            zi(jparci+25-1) = 1
        else if (isto.eq.'NON') then
            zi(jparci+25-1) = 0
        else
            call assert(.false.)
        endif
    endif
!
    call jedema()
end subroutine
