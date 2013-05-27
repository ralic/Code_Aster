subroutine calemn(motfaz, nomaz, iocc, lisi1z, lonli1,&
                  lisi2z, lonli2)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!.======================================================================
    implicit none
!
!      CALEMN   -- CONSTITUTION DE 2 LISTES DE NOMS (K8) DE NOEUDS
!                  LUES RESPECTIVEMENT APRES LES MOTS-CLES
!                  NOEUD_1, GROUP_NO_1, MAILLE_1 OU GROUP_MA_1
!                  D'UNE-PART ET
!                  NOEUD_2, GROUP_NO_2, MAILLE_2 OU GROUP_MA_2
!                  D'AUTRE-PART
!                  CES LISTES NE SONT PAS REDONDANTES
!                  (I.E. LES DOUBLONS SONT ELIMINES)
!                  POUR L'INSTANT, LE SEUL MOT-FACTEUR AUTORISE EST
!                  LIAISON_GROUP
!                  LES NOMS DE CES 2 LISTES SONT LISI1Z ET LISI2Z
!
!   ARGUMENT        E/S  TYPE         ROLE
!    MOTFAZ         IN     K*       MOT-CLE FACTEUR
!                                   = 'LIAISON_GROUP' A CE JOUR
!    NOMAZ          IN     K*       NOM DU MAILLAGE
!    IOCC           IN     I        NUMERO D'OCCURENCE DU MOT FACTEUR
!    LISI1Z         OUT    K*       NOM DE LA LISTE DES NOMS (K8)
!                                   DE NOEUDS LUS APRES LES MOTS-CLES
!                                   NOEUD_1 OU GROUP_NO_1 OU
!                                   MAILLE_1 OU GROUP_MA_1
!    LONLI1         OUT    I        LONGUEUR DE LA LISTE PRECEDENTE
!    LISI2Z         OUT    K*       NOM DE LA LISTE DES NOMS (K8)
!                                   DE NOEUDS LUS APRES LES MOTS-CLES
!                                   NOEUD_2 OU GROUP_NO_2 OU
!                                   MAILLE_2 OU GROUP_MA_2
!    LONLI2         OUT    I        LONGUEUR DE LA LISTE PRECEDENTE
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvtx.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: motfaz, nomaz, lisi1z, lisi2z
! -----  VARIABLES LOCALES
    character(len=8) :: k8bid, noma, typem
    character(len=16) :: motcle, tymocl, motfac
    character(len=24) :: lisin1, lisin2
    integer :: iarg
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!-----------------------------------------------------------------------
    integer :: iocc, lonli1, lonli2, n1, n2, n3, n4
    integer :: n5, n6, n7, n8, nliai
!-----------------------------------------------------------------------
    call jemarq()
!
    motfac = motfaz
    noma = nomaz
    lisin1 = lisi1z
    lisin2 = lisi2z
!
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 9999
!
    n1 = 0
    n2 = 0
    n3 = 0
    n4 = 0
    n5 = 0
    n6 = 0
    n7 = 0
    n8 = 0
!
! --- DETERMINATION DU MOT-CLE A TRAITER POUR LA PREMIERE LISTE
! --- DE NOEUDS (I.E. 'GROUP_NO_1' OU 'NOEUD_1' OU 'GROUP_MA_1'
! --- OU 'MAILLE_1') :
!     --------------
    call getvtx(motfac, 'GROUP_NO_1', iocc, iarg, 0,&
                k8bid, n1)
    if (n1 .ne. 0) then
        motcle = 'GROUP_NO_1'
        tymocl = 'GROUP_NO'
    else
        call getvtx(motfac, 'NOEUD_1', iocc, iarg, 0,&
                    k8bid, n2)
        if (n2 .ne. 0) then
            motcle = 'NOEUD_1'
            tymocl = 'NOEUD'
        else
            call getvtx(motfac, 'GROUP_MA_1', iocc, iarg, 0,&
                        k8bid, n3)
            if (n3 .ne. 0) then
                motcle = 'GROUP_MA_1'
                tymocl = 'GROUP_MA'
            else
                call getvtx(motfac, 'MAILLE_1', iocc, iarg, 0,&
                            k8bid, n4)
                if (n4 .ne. 0) then
                    motcle = 'MAILLE_1'
                    tymocl = 'MAILLE'
                else
                    call u2mesk('F', 'MODELISA2_92', 1, motfac)
                endif
            endif
        endif
    endif
!
! --- CONSTITUTION DE LA PREMIERE LISTE DE NOEUDS :
!     -------------------------------------------
    typem = 'NO_NOEUD'
    call reliem(' ', noma, typem, motfac, iocc,&
                1, motcle, tymocl, lisin1, lonli1)
!
! --- DETERMINATION DU MOT-CLE A TRAITER POUR LA SECONDE LISTE
! --- DE NOEUDS (I.E. 'GROUP_NO_2' OU 'NOEUD_2' OU 'GROUP_MA_2'
! --- OU 'MAILLE_2') :
!     --------------
    call getvtx(motfac, 'GROUP_NO_2', iocc, iarg, 0,&
                k8bid, n5)
    if (n5 .ne. 0) then
        motcle = 'GROUP_NO_2'
        tymocl = 'GROUP_NO'
    else
        call getvtx(motfac, 'NOEUD_2', iocc, iarg, 0,&
                    k8bid, n6)
        if (n6 .ne. 0) then
            motcle = 'NOEUD_2'
            tymocl = 'NOEUD'
        else
            call getvtx(motfac, 'GROUP_MA_2', iocc, iarg, 0,&
                        k8bid, n7)
            if (n7 .ne. 0) then
                motcle = 'GROUP_MA_2'
                tymocl = 'GROUP_MA'
            else
                call getvtx(motfac, 'MAILLE_2', iocc, iarg, 0,&
                            k8bid, n8)
                if (n8 .ne. 0) then
                    motcle = 'MAILLE_2'
                    tymocl = 'MAILLE'
                else
                    call u2mesk('F', 'MODELISA2_93', 1, motfac)
                endif
            endif
        endif
    endif
!
! --- CONSTITUTION DE LA SECONDE LISTE DE NOEUDS :
!     ------------------------------------------
    typem = 'NO_NOEUD'
    call reliem(' ', noma, typem, motfac, iocc,&
                1, motcle, tymocl, lisin2, lonli2)
!
9999  continue
    call jedema()
end subroutine
